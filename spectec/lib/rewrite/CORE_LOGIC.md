# `rewrite` 라이브러리 핵심 로직 — 재구현 설계 기준

> `new-rewrite` 브랜치는 `rewrite` 브랜치(IL → CTRS/Maude)를 재작성하기 위한
> **골격**입니다. 이 문서는 기존 `rewrite` 라이브러리가 **핵심으로 삼았던 로직**을
> 보존합니다 — 골격에서 *지운* 모듈(아래 §5·§6)의 알고리즘까지 포함해, 재구현할 때
> 설계 기준으로 삼기 위함입니다. 운영/빌드 절차와 더 긴 변경 이력은 [CLAUDE.md]를
> 보세요; 이 문서는 *무엇을 왜 그렇게 번역하는가*에 집중합니다.
>
> 현재 골격에 남은 것: 데이터 모델(`rewrite_system.ml`), `to_ctrs`의 심볼/빌더
> 레이어 + 번역 스텁, `simplify`/`to_maude` 스텁, 오케스트레이션
> (`pipeline`/`rewrite`). 지운 것은 `rewrite` 브랜치에서 언제든
> `git checkout rewrite -- <file>` 로 복구할 수 있습니다.

---

## 1. 아키텍처 — 두 파이프라인, 그리고 분기 이유

```
Lang.Il.spec (elaborated)
   │  Defunctionalize  → Simplify → To_ctrs(+Builtin) → Gensym.thread
   ▼
Rewrite_system.t  (CTRS)
   ├─(분석)  string_of_system → Cocoweb(confluence) / Termination(AProVE·MuTerm)
   └─(실행)  Maude_theory.native_system → To_maude → Maude_run → Of_maude
```

**왜 두 파이프라인인가.** 같은 번역 결과를 두 표면이 소비하는데, 스칼라(수·불리언·
문자열) 표현이 서로 달라야 한다:

- **분석(structural)**: CTRS/COPS·TPDB에는 외부 이론이 없다. 그래서 스칼라를
  *자립적 구조*로 인코딩한다 — nat은 Peano `zero`/`succ`, int는 부호-크기
  `int_pos`/`int_neg`, text는 문자 리스트, bool은 자체 `true`/`false`. 산술도
  구조적 재귀 규칙(prelude)으로 정의. 이래야 confluence/termination 도구가 닫힌
  시스템을 본다.
- **실행(built-in theory)**: Maude에는 GMP 기반 `Bool`/`Nat`/`Int`/`String`이 있다.
  그래서 ground 스칼라를 내장 이론 위 wrapper 생성자로 fold하고(`nat(3)`,
  `int(-5)`, `bool(true)`, `txt("E.")`), 손으로 쓴 스칼라 prelude 규칙은 버리고,
  살아남은 연산자를 한 줄 delegation으로 재방출(`eq add(nat(X),nat(Y)) = nat(X+Y)`).
  상수시간 산술.

두 파이프라인은 `Maude_theory.native_system` 지점에서 의도적으로 갈라진다. 분석
표면은 구조적 시스템을 그대로 유지한다.

**`orig`(simplify 이전 spec)를 함께 넘기는 이유:** 타입 정의와 relation 시그니처
(생성자/matcher/subtype 규칙, relation 인자 입출력 분할에 필요)는 *un-simplified*
형태에서 읽어야 한다. 그래서 `To_ctrs.of_spec ~orig:spec (Simplify.simplify_spec spec)`.

---

## 2. 데이터 모델 (`rewrite_system.ml` — 골격에 유지)

```ocaml
type term = Var of string | App of string * term list   (* App(id,[]) 은 bare 출력 *)
type cond = term * term                                  (* term == term *)
type rule = { lhs : term; rhs : term; conds : cond list; owise : bool }
type ctype = SemiEquational | Join | Oriented            (* 우리는 Join 방출 *)
type t = { ctype; vars : string list; rules : rule list; comment : string option }
```

두 텍스트 표면, **의도적으로 다름**:
- `string_of_system` — **COPS**: 머리에 `(CONDITIONTYPE JOIN)`, 조건 `s == t`.
  CoCoWeb·`rewrite` CLI 덤프용.
- `string_of_system_tpdb` — **TPDB**: CONDITIONTYPE 헤더 없음, 조건은 oriented
  `s -> t` 를 ` , ` 로 구분. MuTerm 파서가 COPS 표면에서 *크래시*하므로 termination은
  이 형태를 써야 함.

`slice t ~roots` — `roots`에서 도달 가능한(하향 의존 폐포) 규칙만 남김(심볼별
confluence/termination 검사용). `reachable_heads`/`refs_of_rule`/`defined_head`가
도달성 분석의 기본 연산.

---

## 3. 번역 (`To_ctrs`) — 심장

### 3.1 심볼 네이밍 규약 — *def/use 일치 불변식* (골격에 유지)

심볼을 *정의하는* 규칙과 *쓰는* 모든 규칙이 같은 이름을 만들어야 한다. 그래서 raw
`R.App`/`R.Var` 생성은 이 레이어에만 가둔다.

- `sanitize` — CTRS-safe id로 스크럽(`->`→`minus_gt`, `&&`→`amp_amp` 등). `abbrev`는
  너무 긴 helper 기술자(반복/subtype helper가 본문을 이름에 접음)를 40자 + 8자 해시로
  자름 — *순수 절단은 서로 다른 helper를 병합해 soundness 위험*이라 해시를 붙임.
- **arity를 variant/case 심볼에 접음** (`variant_<origin>_<atoms>_<n>`): 같은 atom
  다른 arity 충돌 제거(예: 대입 `id `= expr` vs 선언 `type id `= expr`). 같은 atom
  *같은* arity 충돌은 아직 가능(§7).
- `func_sym id` = `$`+sanitize, `rel_sym id` = sanitize. 생성자: `variant_sym`,
  `struct_sym`, `field_sym`, `match_sym`, `subty_sym`.
- 수: nat=Peano `zero`/`succ`; int=부호-크기 `int_pos`/`int_neg`(magnitude는 nat).
  리스트 `nil`/`cons`, 옵션 `none`/`some`, text=문자코드별 nullary `chr_<code>` 의
  `cons`/`nil` 리스트(스펙 리터럴이든 백엔드가 인코딩한 프로그램 값이든 *한 모양*).

### 3.2 prelude — 구조적 스칼라 규칙셋

bool(`not`/`and`/`or`/`impl`/`equiv`), Peano nat 산술(`add`/`sub`/`mul`/`div`/`mod`/
`pow`/`leq`/`lt`), 부호-크기 int 산술(`add_int`/…/`sub_int_nat`/`negate_int`/`abs_nat`/
`nat_of_int`), 리스트/옵션 연산(`len`/`cat`/`mem`/`idx`/`slice`/`take`/`drop`/`upd_idx`/
`upd_slice`). int 생성자는 nat과 **disjoint**(공유 `succ` 없음)라 nat `eq`/`succ`가
int 항에 절대 매치 안 됨. `native_replaced_heads`(골격에 유지)가 Maude 백엔드에서
내장 위임으로 대체될 head 목록.

### 3.3 타입유도 규칙 (`defs_of_typ`)

`TypD`마다:
- **variant T**: 케이스 `Ci`마다
  - *matcher*: `match_T_Ci(variant_Oi_Ci(_…)) -> true`, 형제 케이스엔 `-> false`
    (T의 케이스 전체에 대해 total·overlap-free).
  - *subtype*: `subty_T(variant_Oi_Ci(x…)) -> and(sub_pred field_k x_k)` — 페이로드로
    재귀(nullary는 `true`). elaborator가 주입 subtype 케이스를 origin 유지한 채 T로
    평탄화하므로 위계 전체 커버. 비-멤버는 irreducible(양성 사용만).
  - *equality*: 같은 케이스끼리 필드 재귀, 다른 케이스끼린 `false`(T 케이스 범위 내).
- **struct T**: 필드별 접근자, 구조적 `eq`, 자명히 참인 `subty_T`(struct는 invariant).
- **alias T = U**: `subty_T`가 `U`의 검사로 위임.

### 3.4 식/경로 → term (`term_of_exp`)

빌더 레이어 위 디스패치. 신경 쓰는 IL 생성자: `CaseE`(variant), `StrE`(struct),
`OptE`/`ListE`/`ConsE`/`CatE`, `DotE`(필드), `IdxE`/`SliceE`/`UpdE`(경로),
`CallE`(함수호출), `IterE`(§3.7), `MatchE`, 캐스트 `UpCastE`/`DownCastE`/`SubE`(§3.8).
`Upd`의 경로는 중첩 `idx`/`upd_idx`/`upd_field`/`upd_slice` 적용으로 *정적* 컴파일
(평가기 흉내). 수 리터럴: 비음수는 nat magnitude(`peano`), 음수는 `int_neg`.

### 3.5 전제 → 조건 (`conds_of_prem`/`conds_of_prems`)

`prem`을 CTRS **조건**으로:
- `RelPr`(relation 호출): `Mode.partition reltyp args`로 입력/출력 분할 — 입력은 호출
  인자, 출력은 조건의 좌변에 바인딩(`rel_invocation`/`output_term`).
- `IfPr{cond}`: 조건식을 `== true`로.
- `LetPr`: 패턴 바인딩(`pattern_of_exp`로 head 패턴 + 보조 조건).
- `RelAssertPr{call; expect}`: `expect=true`=holds, `false`=does-not-hold.
- `IterPr`(§3.7), `ElsePr`(otherwise — §7의 미구현 갭).

**근사:** `NeOp`/does-not-hold는 분석 표면에서 `== false`로 근사(Join 의미가
근사만 함). 실행(Maude)에서는 `owise` complement로 *totalize*해 결정 가능(§6.2).

### 3.6 절/규칙 → rewrite 규칙 (`rules_of_def`)

- `DecD` 절 `{args; body; prems}` → 규칙: lhs=함수심볼에 args를 *패턴*으로
  (`pattern_of_arg`), rhs=`body` 번역, conds=`prems` 번역.
- `RelD` 규칙 `{ruleid; concl; prems}` → 입력을 lhs 패턴, 출력을 rhs로, prems를 conds로.
- `has_otherwise`로 `ElsePr` 절 식별(§7).

### 3.7 반복 (iterations)

- **`IterE` 값 위치** → `$itermap` helper(요소 타입을 심볼에 포함해 같은 notation의 두
  타입이 한 helper로 collapse되는 것 방지). **binder 위치** → 새 binder + `$unzip` 조건.
- **`IterPr`** → 아무 것도 안 묶으면 `$iterall` 술어; relation 호출 하나를 반복하면
  `$iterapply` map(출력 1개면 스트림 직접, 여러 개면 `$iterproj`); 그 외 바인딩 전제는
  출력별 `$itercollect`.
- **iteration-binder-scope 규율(핵심 버그원).** substitution이 *다른* 반복이 묶은
  요소 변수를 끌고 들어오면 안 됨 — `Simplify.subst_prem`이 `elem_bound`(블록 내 어떤
  반복이든 묶은 요소 변수, `iter_binders_prem`)를 스레딩해 그런 pair를 보류
  (`Prem_env.subst_exp`의 `binds_from` 가드와 짝). 이게 table action-enum
  STREAM-vs-element 버그를 잡음. `Simplify.collapse_rezip_iters`는 unzip→re-zip
  왕복을 단일 반복 변수로 미리 접음.

### 3.8 subtype & cast

- `SubE`(`e <: T`)는 구조적 술어 `sub_pred`(`sub_nat`/`subty_<T>`/`subty_tup`/
  `subty_list`/`subty_opt`, 페이로드 재귀). 양성 사용만 — 비-멤버 `-> false` totality는
  negation 스토리(§7)로 미룸.
- 캐스트: 비-수치는 transparent(faithful). nat↔int는 `Simplify`에서 alias/tuple 해소
  후 `int_pos`/`nat_of_int`로. 단항 마이너스는 magnitude leaf에 `int_pos`를 한 번만
  주입(`yields_int` 가드로 `-(-n)` 이중주입 방지; prelude의
  `negate_int(negate_int x) -> x`가 잔여 상쇄).

### 3.9 prune_unused — 도달성 가지치기

prelude/타입유도 규칙 중 body 규칙에서 도달 불가능한 정의 규칙을 제거(추이적:
`mul`을 남기면 그 rhs의 `add`도 남음). 생성자는 정의 규칙이 없어 절대 안 잘림.

---

## 4. `Simplify` 전처리 + `Prem_env` (지움 — §3.7과 함께 재구현)

`Prem_env.env_of_prems`가 블록 전제로 IL 식 위 **union-find**를 만들어 각 식에 가장
구체적인 canonical 멤버를 준다. `Simplify`는 그 위에서:

(a) 각 변수를 canonical 구체 구조로 치환,
(b) `matches`/필드접근 제약을 head 패턴으로 접음(`reconstruct_pattern`/`hoist_pairs`),
(c) value/let 바인딩 inline,
(d) subtype 전제를 cast로 낮춤,
(e) env가 잉여로 만드는 전제 제거.

**의미 보존 IL→IL 재작성**이라, 결과 spec의 절/규칙이 CTRS 규칙으로 더 직접 매핑됨.
**가장 미묘한 부분은 capture-awareness**(§3.7) — substitution이 다른 binder의 변수를
포획하면 STREAM/element 혼동·orphan 전제가 생김. 디버그: `of_spec`의 2번째 인자로
`Simplify.simplify_spec spec` 대신 `spec`을 주면 simplify를 우회해 버그 출처를 가름.

---

## 5. 지원 패스 (지움 — 재구현 시 참고)

### 5.1 `Defunctionalize` — def-값 인자 specialization
`$f(args, def $g)` → 생성된 1차 복사본 `$f_$g`(`$check := $g`를 템플릿 절들에 치환).
재귀/연쇄 템플릿에 대한 worklist 폐포, 템플릿 제거, 남은 `DefA`는 hard error.
`ctrs_of_spec` **첫 패스**(simplify/translate가 1차만 보게). `DefP` 없으면 identity.

### 5.2 `Gensym` — `$fresh_typeId` 상태 스레딩
상태=마지막 발급 이름, 발급=프라임 덧붙임(seed `"FRESH"`→`FRESH'`→`FRESH''`…, P4
식별자와 충돌 없음). fresh에 닿는 모든 심볼이 후행 상태 인자를 얻고 결과는
`tuple(result, state')`. `ctrs_of_spec` **마지막**(`Gensym.thread`). `Prem_env`가
fresh-닿는 호출을 opaque로 둬 `Simplify`가 발급을 중복 안 함. gensym-free면 identity.

### 5.3 `Builtin` — P4 컬렉션 builtin CTRS 규칙
`BuiltinDecD`가 선언만 하고 `To_ctrs`가 규칙을 안 내는 map/set/list/text builtin에
백엔드-로컬 규칙을 `of_spec`의 `~extra_defs`로 공급(닿는 데서만 유지). 보존된 빌더
레이어(`rule`/`app_t`/`cons_t`/`single_case_ctor`/…) 위에 작성.

---

## 6. 실행 (Maude) 백엔드 (지움 — 재구현 시 참고)

### 6.1 native theory fold (`Maude_theory.native_system`)
분석 시스템 위에서: ground 스칼라를 `nat`/`int`/`bool`/`txt` wrapper로 fold하고,
대체된 스칼라 prelude 규칙(`native_replaced_heads`)을 버림. **내장 sort는 `Val`
바깥**에 둠(kind가 병합되면 import된 연산자 attribute가 충돌). `TextE ""`는 bare
`nil`로 두고 `List < Text` + `eq`/`cat` nil-방정식으로 bridge.

### 6.2 `To_maude` 모듈 방출
order-sorted Maude 시스템 모듈: sort 복구(원본 spec의 시그니처에서), op 선언, eq/rl
프린팅, 내장 delegation 방정식. 함수/prelude→방정식, **입력-moded relation→방정식**
(`Maude_run`이 결정적으로 `reduce`), 그 외 relation→규칙. negated no-output 판정은
guarded `owise` complement로 *totalize*(does-not-hold가 실행에서 결정 가능). 심볼
spelling은 `maude_sym`(`Run_prog`→`Run-prog`).

### 6.3 META-TERM reflection (`meta_term_of_value`/`meta_start_app` + `Maude_run`)
**왜:** 거대 mixfix object 문법으로 *시작항*을 파싱하는 게 프로그램당 ~6.9s 지배
비용이었음. 대신 고정·소형 **META-TERM** 문법으로 적어
`metaReduce(upModule('SPEC,false), <meta항>)`로 돌림(`foo(a,b)`→`'foo[a,b]`,
0-arity→`'foo.Sort`; nat은 `'nat['s_^N['0.Zero]]`, 음수 int `'-_[..]`, bool/txt는
`'true.Bool`/`'".."`). 첫 `metaReduce`가 50k줄 모듈 내부화 1회(~10.4s) 후 같은
invocation의 모든 프로그램은 ~4ms. `downTerm`으로 object 항 복원해 기존 파싱 재사용.
`run_batch`는 시작항 리스트를 **maude 1회**로(센티넬 구분) — 모듈 내부화 상각.

### 6.4 `Of_maude` 역번역 오라클 — 결과-VALUE 비교
verdict(PASS/STUCK) 일치를 넘어, 두 엔진이 받는 프로그램의 *타이핑 결과값*을 비교.
Maude normal form을 IL value로 역번역(spec에서 읽은 forward 테이블; `variant_sym`/
`struct_sym` spelling이 lossy라 역인덱스 필요). `values_of_result`가 gensym
`tuple(result,state)` 래퍼를 벗김. `canonicalize`를 **양쪽 모두**에 적용:
(1) `FRESH…` 잎 이름 정규화(두 gensym 모델 비교), (2) `map<K,V>` 엔트리를 키
`Value.compare`로 정렬(맵은 unordered — interp은 `VMap.bindings` 정렬, 번역은 삽입
순서). 진짜 내용 차이는 그대로 드러냄.

### 6.5 외부 도구 브리지 (`Cocoweb`/`Muterm`/`Aprove`/`Termination`)
직렬화→temp 파일→클라이언트 호출→verdict 매핑(`Yes|No|Maybe|Timeout|Error`).
- `Cocoweb`/`Muterm`: Python 클라이언트가 웹 인터페이스에 POST.
- `Aprove`: 로컬 `aprove.jar` 직접 실행(`-m wst -t N`).
- `Termination` **디스패처**: `Rewrite_system.is_unconditional` → 무조건이면 AProVE,
  조건부면 MuTerm. CoCoWeb=confluence만, MuTerm=termination만 보고. `Timeout`은
  `Maybe`와 구분 유지.

---

## 7. 알려진 gap / 근사 (재구현 시 주의)

- **`otherwise`(`ElsePr`) 미처리** → fallthrough 절이 "앞 절 미적용" 가드를 잃고 앞
  규칙과 overlap → **non-confluent**(예: `$lookup`이 같은 LHS 두 규칙 방출). Fix=앞
  절 가드들의 부정으로 번역. equality/`matches` 가드는 깔끔하나 relation-전제 가드는
  negation 벽(relation은 값 반환, 실패=stuck). Maude 표면은 `owise`로 totalize됨,
  CTRS/COPS는 여전히 근사.
- **`NeOp`/does-not-hold = `== false` 근사**(§3.5) — 위와 같은 뿌리.
- **sanitizer 같은-atom 같은-arity 충돌**, relation-vs-prelude 네임스페이스, 단항
  `term_of_num` (§3.1 잔여).
- div-by-zero·out-of-bounds는 irreducible로 남김(부분 규칙).

---

## 8. 재구현 마일스톤

- **M1 — 분석 골든.** `Simplify.simplify_spec` + `To_ctrs.of_spec`(prelude,
  `defs_of_typ`, `term_of_exp`, `conds_of_prem`, `rules_of_def`, 반복/subtype helper,
  `prune_unused`) 재구축. 검증:
  `main.exe rewrite specs/impty/base/spec.spectec | diff - spec.rewrite`.
- **M2 — Maude 실행.** `To_ctrs.var_type_hints`, native theory fold(§6.1),
  `To_maude.*`(§6.2–6.3). `Maude_run`로 impty 실행 확인.
- **M3 — 결과-VALUE 오라클.** `Of_maude.*`(§6.4). same-spec interp vs Maude 비교.

[CLAUDE.md]: ./CLAUDE.md
