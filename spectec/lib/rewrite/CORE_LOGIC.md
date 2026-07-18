# `rewrite` 라이브러리 핵심 로직 — 재구현 설계 기준

> `new-rewrite` 브랜치는 `rewrite` 브랜치(IL → CTRS/Maude)를 재작성하기 위한
> **골격**입니다. 이 문서는 기존 `rewrite` 라이브러리가 **핵심으로 삼았던 로직**을
> 보존합니다 — 골격에서 *지운* 모듈(아래 §5·§6)의 알고리즘까지 포함해, 재구현할 때
> 설계 기준으로 삼기 위함입니다. 운영/빌드 절차와 더 긴 변경 이력은 [CLAUDE.md]를
> 보세요; 이 문서는 *무엇을 왜 그렇게 번역하는가*에 집중합니다.
>
> 현재 상태: 데이터 모델(`rewrite_system.ml`), `to_ctrs`의 심볼/빌더 레이어 +
> 번역 스텁, 오케스트레이션(`pipeline`/`rewrite`)은 온전. **지원 패스(§5:
> `exp_map`/`defunctionalize`/`gensym`/`builtin`)와 Maude 백엔드(§6:
> `to_maude`/`maude_run`/`of_maude`/`maude_theory`)는 `rewrite` 브랜치에서 복구해
> 컴파일·배선 완료** — `of_spec` 스텁에 막혀 런타임은 아직 `failwith`. **`simplify`는
> 이 프로젝트에서 의도적으로 identity**(§4), 그래서 `prem_env`는 재구현하지 않습니다.
> **남은 스텁은 `to_ctrs`의 `of_spec`·`var_type_hints` 둘뿐.** 여전히 지운 것
> (`cocoweb`/`muterm`/`aprove`/`termination`)은 `git checkout rewrite -- <file>`로
> 복구 가능.

---

## 1. 아키텍처 — 두 파이프라인, 그리고 분기 지점

```
Lang.Il.spec (elaborated)
   │  Defunctionalize → Simplify → To_ctrs.of_spec ~scalars:?? (+Builtin) → Gensym.thread
   │                                                      ▲
   │                          스칼라 이론이 유일한 분기점 ──┘
   ├─(분석)  of_spec ~scalars:Structural → Rewrite_system.t
   │            → string_of_system_maude → Mfe(CRC confluence + ChC coherence)
   └─(실행)  of_spec ~scalars:Native   → Rewrite_system.t   ← ② 재-fold 없음, 직접 생성
                → To_maude → Maude_run → Of_maude
```

**핵심: 두 경로는 *같은* 구조적 번역을 공유하고, 스칼라 이론
(`To_ctrs.scalar_theory = Structural | Native`) 하나에서만 갈라진다.** 그래서
Maude 시스템은 구조적 시스템을 *다시 fold하는 별도 패스(옛 `Maude_theory`)* 없이
번역 단계에서 **직접** 만들어진다. variant/struct/relation/함수 규칙은 두 경로에서
동일하고, 스칼라 leaf와 prelude만 달라진다.

**왜 두 파이프라인인가.** 같은 번역 결과를 두 표면이 소비하는데, 스칼라(수·불리언·
문자열) 표현이 서로 달라야 한다:

- **분석(structural)**: CTRS/COPS·TPDB에는 외부 이론이 없다. 그래서 스칼라를
  *자립적 구조*로 인코딩한다 — nat은 Peano `zero`/`succ`, int는 부호-크기
  `int_pos`/`int_neg`, text는 문자 리스트, bool은 자체 `true`/`false`. 산술도
  구조적 재귀 규칙(prelude)으로 정의. 이래야 confluence/termination 도구가 닫힌
  시스템을 본다.
- **실행(built-in theory)**: Maude에는 GMP 기반 `Bool`/`Nat`/`Int`/`String`이 있다.
  그래서 ground 스칼라를 내장 이론 위 wrapper 생성자로 인코딩하고(`nat(3)`,
  `int(-5)`, `bool(true)`, `txt("E.")`), 손으로 쓴 스칼라 prelude 규칙
  (`native_replaced_heads`)은 *처음부터 안 내고*, `To_maude`가 한 줄 delegation으로
  방출(`eq add(nat(X),nat(Y)) = nat(X+Y)`). 상수시간 산술.

두 파이프라인은 **`of_spec`의 `~scalars` 인자**에서 갈라진다. `Structural`이면 위
구조적 인코딩+prelude, `Native`면 wrapper 인코딩+prelude 생략. **별도 fold 패스는
없다** — 옛 설계의 `Maude_theory.native_system`(구조적 시스템을 받아 다시 fold)은
이 seam으로 대체돼 삭제됐다. 분석 표면은 구조적 시스템을 그대로 유지한다.

**`orig`(simplify 이전 spec)를 함께 넘기는 이유:** 타입 정의와 relation 시그니처
(생성자/matcher/subtype 규칙, relation 인자 입출력 분할에 필요)는 *un-simplified*
형태에서 읽어야 한다. 그래서 `To_ctrs.of_spec ~orig:spec (Simplify.simplify_spec spec)`.
**단, 이 프로젝트에서 `Simplify.simplify_spec`은 identity(§4)**라 `orig`와 번역 대상이
같은 스펙이다 — `of_spec`는 un-simplified IL을 직접 번역한다.

---

## 2. 데이터 모델 (`rewrite_system.ml` — 골격에 유지)

```ocaml
type term = Var of string | App of string * term list   (* App(id,[]) 은 bare 출력 *)
type cond = term * term
type rule = { lhs : term; rhs : term; conds : cond list; owise : bool }
type t = { vars : string list; rules : rule list }
```

**텍스트 표면은 하나** — `string_of_system_maude ~rule_heads`(**Full Maude 시스템 모듈**,
단일 sort `Term`): 등식 fragment는 `eq`/`ceq`, `rule_heads`(비입력-moded relation,
`To_ctrs.rule_head_syms`)는 `rl`/`crl`. MFE의 CRC(등식 confluence)/ChC(rl coherence)가
소비(§6.5).

> 옛 COPS(`string_of_system`)·TPDB(`string_of_system_tpdb`) 표면과 `ctype`/`comment`
> 메타데이터·`is_unconditional`은 **삭제**됐다(소비자였던 CoCoWeb·AProVE·MuTerm 전부
> 제거). 분석 confluence는 이제 MFE 한 경로뿐.

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

CTRS에는 바인더/컴프리헨션이 없으므로 IL의 반복(`*`/`?`)은 전부 **이름 붙은 1차
재귀 헬퍼**로 컴파일된다. 헬퍼 이름은 내부 전제/본문의 프린트 문자열 + iter 종류 +
bound 개수에서 유도(`iter_helper_sym`)되어 구조적으로 같은 반복은 헬퍼를 공유하고
(`Helper_defs`가 심볼로 dedup), `*`는 `nil`/`cons`, `?`는 `none`/`some` 변형을 가진다.

기반 개념 — `iter_split`이 반복 변수를 둘로 나눈다: **bound**(이미 값이 있는 입력
스트림; 반복이 걷는 spine이 되고, 여러 개면 lockstep으로 동시 cons 매칭) vs
**binding**(스텝마다 생산되어 리스트로 수집되는 출력). 루프-불변 자유변수(fvs)는
선두 인자로 스레딩된다.

**`IterPr`(반복 전제) → 2분기** (`conds_of_prem`의 dispatch; 헬퍼 규칙 생성은
`iterpr_defs`):

- **`$iterall`** — binding이 없을 때(순수 검사). "모든 원소에서 전제 성립"을 재귀로:

  ```
  $iterall(fvs, nil, nil)               = true
  $iterall(fvs, cons(a,as), cons(b,bs)) = $iterall(fvs, as, bs)   if <원소 전제>
  ```

  사용처 조건은 `$iterall(..) = true`. 예: `(Type_alpha: field_a ~~ field_b)*`
  (무출력 relation은 모든 인자가 bound → 이 갈래). cons-스텝이 **조건부라 partial**:
  원소 하나가 실패하면 false가 아니라 **stuck**.
- **`$itercollect`** — 수집이 있으면 **전제당 헬퍼 정확히 하나**. 수집 성분들
  (comps — 단일 relation call이면 그 출력 순서, 아니면 binding 순서;
  `iter_collect_components`가 접미사·튜플 순서·등록 순서의 단일 진실 공급원)의
  스트림을 반환한다. 원소는 k=1이면 성분 자체, k≥2면 **성분 튜플**:

  ```
  -- 일반형 (조건부; 내부 전제를 스텝당 1회만 평가)
  $itercollect(fvs, nil)                    = nil
  $itercollect(fvs, cons(<원소패턴>, rest)) = cons(tuple(b1_h,…,bk_h), $itercollect(fvs, rest))   if <원소 전제>
  -- ex-apply 특수화: 내부가 단일 relation call이고 수집이 정확히 그 출력일 때
  -- (iter_call_map). 원소 = call 인라인, 무조건 eq — 원소당 호출 정확히 1회
  -- (effectful/gensym 반복의 불변식, todo.md 2026-07-02 참조)
  $itercollect(fvs, cons(x,xs)) = cons(R(..x..), $itercollect(fvs, xs))
  ```

  사용처 조건: k=1이면 `($itercollect(ins), b*)` — 스트림이 그대로 바인딩.
  k≥2면 튜플 스트림을 신선한 `iterbind_N`으로 받고, 각 성분 `bi`는 head binder와
  **정확히 같은** 취급을 받는다(§3.7.1): 소비 헬퍼가 있으면 튜플 스트림을 직접
  destructure(fused spine), escape하면 `($iterproj_bi(iterbind_N), bi*)` 조건으로
  복원, absorbed+dead면 prune. 합성 튜플 본문 `(b1,…,bk)`(`collect_tuple_body`)이
  등록(`iter_ctx`)·명명(`iter_proj_sym`)·정의(`elem_pat_of_binder`)를 한 곳에서
  결정한다. 조건부 스텝(일반형)은 `$iterall`처럼 partial.

**`IterE`(반복 식)** 는 위치에 따라:

- **값 위치** → `$itermap` helper(성분 스트림들 → 구조 리스트; 요소 타입을 심볼에
  포함해 같은 notation의 두 타입이 한 helper로 collapse되는 것 방지).
- **binder(패턴) 위치** → 컬렉션 전체를 신선한 `iterbind_N`으로 받는다(CTRS LHS는
  생성자만 허용하므로 구조를 가진 반복 본문 — 예: head 인자 `(typeId typeIR)*`,
  쌍의 리스트 — 은 패턴이 될 수 없다). 공동-반복 변수 v를 어떻게 복원하는지는 v가
  소비되는 방식에 달렸다(§3.7.1의 fusion): 소비 헬퍼가 있으면 `iterbind_N`을 직접
  destructure(projection 없음), **escape**하면 `$iterproj_v`로 복원.

  ```
  $iterproj_v(fvs, cons(<원소패턴>, rest)) = cons(v_h, $iterproj_v(fvs, rest))
  ```

  원소가 `<원소패턴>`에 안 맞으면 stuck(refutable), 캡처 fvs가 원소 안에서 매칭되면
  비좌선형. `$itermap`의 역방향. k≥2 `$itercollect`의 합성 튜플 스트림도 같은
  `$iterproj` 계열로 복원된다(그 경우 원소패턴이 bare 변수 튜플이라 irrefutable).

| 헬퍼 | 트리거 | 성격 | total? |
|---|---|---|---|
| `$iterall` | IterPr, 수집 없음 | 전 원소 검사 → `true` | ✗ (실패=stuck) |
| `$itercollect` | IterPr, 수집 있음 | 전제당 1개; k≥2면 튜플 스트림; 단일 rel call이면 무조건 map | ex-apply 특수화만 ✓ |
| `$itermap` | IterE 값 위치 | 성분 스트림들 → 구조 리스트 | ✓ (형태 맞으면) |
| `$iterproj` | 구조 스트림의 **escape** 축 (head binder / k≥2 collect 튜플) | 구조 리스트 → 성분 스트림 복원 | 본문에 따라 (임의 본문 refutable / bare 튜플 irrefutable) |

partial(✗) 헬퍼들의 "실패=stuck" 거동은 실행 표면에선 의도된 것이지만, 분석
표면의 owise/negation 반사에서는 total boolean 짝(`and`-fold + 길이 불일치
`false`)이 필요해지는 지점이다 — §7과 [todo.md](todo.md)의 negation 스토리 참조.
- **iteration-binder-scope 규율(핵심 버그원).** substitution이 *다른* 반복이 묶은
  요소 변수를 끌고 들어오면 안 됨 — `Simplify.subst_prem`이 `elem_bound`(블록 내 어떤
  반복이든 묶은 요소 변수, `iter_binders_prem`)를 스레딩해 그런 pair를 보류
  (`Prem_env.subst_exp`의 `binds_from` 가드와 짝). 이게 table action-enum
  STREAM-vs-element 버그를 잡음. `Simplify.collapse_rezip_iters`는 unzip→re-zip
  왕복을 단일 반복 변수로 미리 접음.

#### 3.7.1 co-iteration의 projection fusion (종료 위한 SoA→AoS)

co-iteration `(v n)*`를 head binder에서 받을 때, 예전에는 **축별 분리(SoA)**로
컴파일했다: `iterbind_N`을 통짜로 받은 뒤 축마다 `$iterproj_v(iterbind_N)` /
`$iterproj_n(iterbind_N)`으로 독립 스트림을 만들고, 소비 헬퍼가 그 스트림을
소비했다. 그러면 소비 헬퍼의 재귀가 `$iterproj_v(iterbind_N)`이라는 **함수 호출
결과**의 tail에 걸려, AProVE의 dependency-pair 분석이 감소를 syntactic subterm으로
못 본다(→ 종료 증명 실패).

**fusion(원소 단위, AoS)**: 소비 헬퍼가 projection 스트림 대신 **원본 `iterbind_N`을
직접 받아** 자기 정의에서 head 원소 패턴 `cons(<원소패턴>, rest)`로 destructure한다.
재귀가 `rest`(= 원본 리스트의 syntactic subterm)로 내려가 종료가 드러난다.
의미 보존은 순수 map-fusion 항등식 `zip(map(f, unzip_v L), unzip_n L)
≡ map(λ(v,n).(f v, n)) L`.

구현(`to_ctrs.ml`):

- **binder 레지스트리**(`iter_ctx`, rule 단위 스레딩). `pattern_of_exp`가 head
  `IterE`를 만나면 각 공동-반복 변수를 `{iterbind, body, vars}`로 등록하고
  projection 조건을 **일단 제자리에 방출**해 둔다(순서 보존).
- **소비 지점**(`term_of_exp`의 `IterE`, `conds_of_prem`의 `IterPr`)이 `spines_of_ids`로
  spine을 계산한다: 등록된 변수는 같은 `iterbind`끼리 하나의 **fused spine**(원본
  리스트를 인자로, 정의에서 `cons(elem_pat, rest)` destructure)으로, 미등록 변수는
  기존대로 **bare spine**(`cons(hd, tl)`)으로. 흡수한 변수는 `absorbed`에 기록.
- **escape/dead fallback**(`prune_absorbed_projs`). rule 조립 후, `absorbed`이면서
  최종 항 어디에도 자유롭게 남지 않은 변수의 projection 조건만 제거한다. 다른
  함수(예: `variant-set`/`$partition`/`$distinct`/`len`)로 흘러가 자유로 남는
  (**escape**) 변수, 또는 흡수되지 않은 dead 변수는 projection을 그대로 유지 →
  `$dom_map`/`$codom_map` 등은 무변화.
- **심볼 네이밍**(`spine_disamb`). fused spine이 하나라도 있으면 헬퍼 이름에
  spine별 태그(bare `b` / fused `f<body>`)를 붙여, 같은 내부 전제라도 소스가
  binder/bare로 갈리는 rule 간 dedup 충돌을 막는다. all-bare면 태그 없이 기존
  이름을 유지(무변화).
- **정의 방출은 반드시 그 호출 사이트의 ctx로**(`iter_helper_defs`). 헬퍼 심볼이
  `spines_of_ids ctx`에 의존하므로, 정의를 방출하는 walk가 호출부와 다른 ctx를
  쓰면 이름이 갈려 "호출됐는데 정의 없음"이 된다(정의된 쪽은 아무도 안 불러
  `prune_unused`가 지우므로 흔적도 안 남는다). 규율: rule 자신의 head/result/
  premise 위치는 `Some ctx`, **iterated 전제의 inner는 `None`** — 그 자리의 조건은
  `iterpr_defs`가 레지스트리 없이(헬퍼 안에는 head binder가 없고 spine이 인자로
  들어온다) 컴파일하기 때문이다. fusion 도입 당시 이 비대칭이 실제 회귀를 냈다
  (중첩 `$itermap` 3건 미정의 → `Cast_expl_neq/structTypeIR` 등 발화 불가;
  2026-07-18 수정, [todo.md](todo.md) 참조).

**premise-side 확장 (2026-07-18, 헬퍼 패밀리 통합).** 위 기계를 `IterPr`의 출력
쪽에도 그대로 적용해 세 갈래를 통합했다:

- **`$iterapply`/`$iterproj`(구, 전제 기반) → `$itercollect`로 흡수.** ex-apply는
  merged `$itercollect`의 무조건 특수화가 됐고(원소당 호출 1회 불변식 유지 —
  todo.md 2026-07-02 "gensym 벽"의 교훈), 다출력 튜플 스트림의 성분 복원은 별도
  패밀리 대신 **합성 튜플 본문 `(b1,…,bk)` 위의 `$iterproj`**(head-side와 같은
  `spine_projection_rules`)가 맡는다. 구 `$iterproj($iterapply(…))` — 함수 호출
  결과 위의 재귀, 정확히 SoA 잔재 — 패턴이 소멸했다.
- **변수별 수집 → 튜플 수집.** k≥2 일반 collect는 성분마다 spine을 재주사하며
  같은 조건을 k회 평가하던 것을, 성분 튜플을 모으는 헬퍼 하나(조건 1회)로
  합쳤다. 사용처는 튜플 스트림을 `iterbind_N`으로 받아 head binder와 동일하게
  등록 → 소비 헬퍼 fused destructure / escape `$iterproj` / dead prune.
- **`$unzip` → `$iterproj` 개명.** head-side escape 헬퍼와 premise-side 튜플
  projection이 한 패밀리가 되면서 이름도 통일(`$iterall`/`$itercollect`/
  `$itermap`/`$iterproj`). reflect.ml의 게이트는 `["$iterall"; "$itercollect"]`로
  줄고, `gen_itercollect_holds`가 **형태 기반**으로 두 모양을 처리한다(스텝
  무조건+원소가 relation call → `holds_R` 반사; 그 외 → 조건 and-fold. 단,
  `fold_premise_binders`가 let-destructure 조건을 LHS 패턴으로 접으면 일반
  collect도 무조건이 될 수 있으므로 "무조건이면 apply형"으로 판정하면 안 된다 —
  원소가 relation call인지가 기준). owise 반사는 통합 전과 동일(69 reflected /
  3 complement-enumerated / 0 kept).

**효과와 한계.** (2026-07-17 head-side fusion 시점의 실측; 당시 이름 `$unzip`,
현 `$iterproj`.) projection 소비 rule이 fused로 접혀 corpus의 `op $unzip`이 105→45로
줄고(escape 잔존분만 남음), 비-iter 심볼 rule은 무변화(전체 differential에서 iter
헬퍼 외 0라인). 최소 예시 toy(단순 self-recursive)·toy2(3함수 상호재귀 미러)는
fused 인코딩으로 AProVE **YES**를 실측 — 종료 구조가 실제로 개선됨을 확인. 다만
실제 `$invalidate_value`/`$invalidate_headerUnion`은 fused 후에도 term **MAYBE**
유지인데, 원인은 unzip이 아니라(subty guard 제거·`$unzip` 소거·큰 signature 축소·
JVM 힙 확대·yices 부재를 **전부 배제**해도, prune 후 8-rule/12-op 극소 슬라이스에서도
MAYBE) AProVE 자동 전략이 이 특정 구조의 종료 증명을 못 찾는 **도구 측 한계**다
(동형 구조의 toy2는 YES). 즉 fusion은 종료를 논리적으로 보장하고 규칙 수를 줄이는
정당한 개선이지만, 이 대상들의 AProVE 판정을 MAYBE→YES로 바꾸지는 못했다.

**premise-side 통합 후 재검증(2026-07-18).** 통합된 튜플-collect + `$iterproj`
escape를 포함하는 슬라이스가 AProVE termination **YES**를 실측(`$callableId_IR`
다출력 ex-apply, `$dom_map`) — 재귀가 원본 리스트의 구문적 tail로 하강함이 통합
후에도 유지됨을 확인. 대상 2심볼은 MAYBE 유지(위 도구 한계, 통합 전과 동일 =
회귀 아님). MFE CRC는 head-side `$invalidate_headerUnion`·premise-side k=1
`$is_default_parameterIR` 모두 YES/YES; k≥2 튜플 collect 소비자(`$resolve_constraint`
등)는 typing/constraint relation을 끌어와 CRC TIMEOUT — **통합 직전 커밋으로 대조해도
동일 TIMEOUT이라 회귀 아님**(심볼 특유 blowup). 이들의 의미 보존은 전 corpus 실행
differential 25/26 MATCH·structural 표본 MATCH로 실행 기반 뒷받침.

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

### 3.10 재구현 로드맵 — `of_spec`·`var_type_hints` (남은 스텁 두 개)

골격은 **심볼/빌더 레이어 전체**(`sanitize`·`*_sym`·`*_t`·`rule`/`rule_cond`·
`single_case_ctor`/`case_ctor` 등)와 **thin 질의**(`def_symbols`·
`input_moded_rel_syms`·`rule_head_syms`·`split_inputs`·`native_replaced_heads`)가
이미 온전하다. 채울 것은 *번역 본체* 둘뿐. 권위 있는 참조는 `rewrite` 브랜치:
`git show rewrite:spectec/lib/rewrite/to_ctrs.ml` (총 1966줄; 아래 줄번호는 그 파일).

**`var_type_hints`** (참조 1850–1886; 쉬움, 먼저) — `scalars`/`Simplify` 무관.
헬퍼 3개를 함께 포팅: `collect_var_types`(1755)·`collect_prem_var_types`(1804)·
`resolve_var_types`(1833). `VarE` note에서 변수별 타입을 모으고 occurrence가 충돌하면
그 변수를 버린다. `To_maude`가 변수의 narrow 타입 복원에 쓴다.

**`of_spec`** (참조 1923–1944) 가 부르는 헬퍼 DAG (전부 `rewrite` 브랜치에 있음):

```
of_spec
├─ prelude            (776)   구조적 스칼라 규칙셋 (bool/nat/int/list/opt)
├─ defs_of_typ        (1053)  TypD → matcher/subtype/eq/accessor 규칙 (§3.3)
├─ rules_of_def       (1729)  DecD 절·RelD 규칙 → body 규칙 (§3.6)
│   ├─ rule_of_clause (1704)  ├ pattern_of_arg(1689)/term_of_exp(574)/conds_of_prems(1403)
│   └─ rule_of_rel_rule(1715) └ split_inputs/output_term(1160)/rel_invocation(1164)
├─ iter_helper_defs   (1568)  IterE/IterPr 헬퍼 (§3.7)
├─ sub_helper_defs    (1598)  subty_tup/list/opt 헬퍼 (§3.8)
├─ char_eq_rules      (1917)  텍스트 바이트 alphabet 위 eq
└─ prune_unused       (1897)  body 도달 불가능한 정의 규칙 제거 (§3.9)
```

`term_of_exp`(574, §3.4)와 `conds_of_prem`(1333, §3.5)이 본체의 대부분이다.

**`rewrite` 브랜치 대비 반드시 다른 3가지:**

1. **결과 레코드.** 데이터 모델이 `{ vars; rules }`로 바뀜(§2; `ctype`/`comment`
   삭제). 마지막 줄: 옛 `{ R.ctype = R.Join; vars; rules; comment = None }` →
   새 `{ R.vars; rules }`.
2. **`~scalars` 분기 (옛 `of_spec`엔 없던 인자).** 옛 설계는 항상 Structural을 내고
   `maude_theory.native_system`이 후처리 fold했지만 그 fold는 삭제됐다(§6.1). 이제
   `of_spec`가 **딱 두 군데만** 분기한다:
   - *리터럴 leaf*: `term_of_num`/`text_t`/bool 리터럴이 `Structural`이면 Peano
     `peano_of_int`/char-list `text_t`/`true_t`, `Native`면 `Maude_theory.nat_t`/
     `int_t`/`text_t`/`bool_t`(wrapper). **연산자 적용(`add`/`cat`…)은 양쪽 동일**
     (To_maude가 Native에서 delegation으로 채움).
   - *prelude·char*: `Native`면 `prelude`에서 `native_replaced_heads`에 속한 head의
     정의 규칙을 빼고, `char_eq_rules` 닫힘도 생략(Native 텍스트는 `txt("…")`
     리터럴이라 `chr_` 상수가 없음).
   구현 팁: `term_of_exp`/`prelude`에 `scalars`를 thread해 분기를 leaf 한 곳에 가둘
   것 — 나머지 구조 번역은 두 경로가 공유한다(generic 목표).
3. **`Simplify`가 identity ⇒ `of_spec`가 un-simplified IL을 직접 받음**(§4).
   `simplified` 인자 == `orig`. 옛 `term_of_exp`/`conds_of_prem`은 Simplify가 이미
   (변수 전개·`matches`/필드접근 head 패턴 fold·subtype→cast·잉여 전제 제거)를 했다고
   가정했다. 그 가정이 사라졌으니 generic 번역은 `MatchE`·필드접근 전제·`SubE`를
   `of_spec` 안에서 직접 다뤄야 한다(§4의 목록 = of_spec가 대신 감당할 정규화
   체크리스트). **이번 재작성의 진짜 난점.**

**검증 순서:** Structural 먼저 채워 빌드 → `[@@@warning "-32-69"]`(to_ctrs.ml 상단)
제거 → impty/base 골든(`rewrite --ctrs … | diff - spec.ctrs`; Simplify=identity라
옛 골든과 다를 수 있음, 의도된 차이면 갱신) → 그 다음 Native 분기 추가(To_maude/
Maude_run이 이미 대기).

---

## 4. `Simplify` — 이 프로젝트에서는 identity (`Prem_env` 미재구현)

**`Simplify.simplify_spec`은 이 프로젝트에서 의도적으로 identity다 — 스펙을 그대로
반환한다.** `To_ctrs`가 유일한 번역 표면이고, 옛 단순화가 하던 정규화를 generic한
`of_spec`가 직접 감당하게 하려는 설계 결정이다. 그래서 `Simplify`만 먹이던
`Prem_env`(union-find 엔진)도 **재구현하지 않는다.**

> **설계 함의:** `of_spec`는 옛 설계가 단순화에 의존하던 형태를 더 이상 기대할 수
> 없다. 변수 전개·`matches`/필드접근의 head 패턴 fold·value/let inline·subtype→cast·
> 잉여 전제 제거 — 이 정규화가 필요하면 `of_spec`가 번역 중에 처리하거나, 그것을
> 전제하지 않는 번역이어야 한다(generic 목표와 부합).

아래는 옛 `rewrite` 브랜치 `Simplify`/`Prem_env`가 **무엇을** 했는지의 참고
(재도입은 안 하지만, `of_spec`가 대응해야 할 정규화의 목록):
`Prem_env.env_of_prems`가 블록 전제로 IL 식 위 union-find를 만들어 각 식에 canonical
멤버를 주고, `Simplify`가 (a) 변수→canonical 구조 치환, (b) `matches`/필드접근을 head
패턴으로 fold, (c) value/let inline, (d) subtype→cast, (e) 잉여 전제 제거를 했다.
가장 미묘한 부분은 capture-awareness(§3.7)였다.

---

## 5. 지원 패스 (복구됨 — `pipeline.ml`의 공통 `build`에 배선)

세 패스 모두 `rewrite` 브랜치에서 복구돼 분석·실행 두 경로 공통으로 wrap된다
(`Pipeline.build scalars spec`: Defunctionalize FIRST → `To_ctrs.of_spec
~extra_defs:(Builtin.rules_of_builtins spec)` → `Gensym.thread` LAST). 각 패스는
대응 기능이 없는 spec(impty: def-파라미터·컬렉션 builtin·gensym 모두 없음)에
identity라 골든에 무영향. `of_spec`/`Simplify`가 스텁인 동안은 런타임이 거기서
멈추지만, 채워지면 즉시 효과가 난다. `Defunctionalize`는 복구된 `Exp_map`(IL 얕은
traversal: `map_subexps`/`subexps`/`exps_of_prem`)을 쓴다.

### 5.1 `Defunctionalize` — def-값 인자 specialization
`$f(args, def $g)` → 생성된 1차 복사본 `$f_$g`(`$check := $g`를 템플릿 절들에 치환).
재귀/연쇄 템플릿에 대한 worklist 폐포, 템플릿 제거, 남은 `DefA`는 hard error.
`build` **첫 패스**(simplify/translate가 1차만 보게). `DefP` 없으면 identity(물리적
동등성 1-slot memo).

### 5.2 `Gensym` — `$fresh_typeId` 상태 스레딩
상태=마지막 발급 이름, 발급=프라임 덧붙임(seed `"FRESH"`→`FRESH'`→`FRESH''`…, P4
식별자와 충돌 없음). fresh에 닿는 모든 심볼이 후행 상태 인자를 얻고 결과는
`tuple(result, state')`. `build` **마지막**(`Gensym.thread`). gensym 루트 목록은
`Gensym.gensym_ids`가 자체 보유(Prem_env 미복구) — `Prem_env`/`Simplify` 복구 시
fresh-닿는 호출을 opaque로 둬(`Simplify`가 발급을 중복 안 하게) 이 목록을 그대로
재사용해야 한다. gensym-free면 identity. 디버그용 `Rewrite_system.string_of_rule`
(COPS 풍 `lhs -> rhs | s == t` 렌더)을 에러 메시지에 사용.

### 5.3 `Builtin` — P4 컬렉션 builtin CTRS 규칙
`BuiltinDecD`가 선언만 하고 `To_ctrs`가 규칙을 안 내는 map/set/list/text builtin에
백엔드-로컬 규칙을 `of_spec`의 `~extra_defs`로 공급(닿는 데서만 유지). 보존된 빌더
레이어(`rule`/`app_t`/`cons_t`/`single_case_ctor`/…) 위에 작성.

---

## 6. 실행 (Maude) 백엔드 (복구됨 — `to_maude`/`maude_run`/`of_maude`/`maude_theory`)

> **상태:** 네 모듈 모두 `rewrite` 브랜치에서 복구돼 컴파일·배선 완료. `of_spec` 스텁이
> 채워지면 즉시 동작한다. `module_of_spec`은 `Pipeline.maude_system_of_spec`
> (= `of_spec ~scalars:Native`)를 직접 호출한다.

### 6.1 native 스칼라 이론 — `of_spec ~scalars:Native` (별도 fold 패스 아님)
**옛 설계**는 분석 시스템을 받아 다시 fold하는 `Maude_theory.native_system` 패스를
뒀지만(②), **새 설계는 그 패스를 없애고** 번역 단계에서 직접 native 항을 낸다:
`of_spec`가 `~scalars:Native`면 ground 스칼라를 `nat`/`int`/`bool`/`txt` wrapper로
인코딩하고, 대체될 스칼라 prelude(`native_replaced_heads`)를 *처음부터 안 낸다*
(To_maude가 delegation으로 채움). 의미·불변식은 동일하고, 중복 패스만 제거. 재구현
시 `of_spec`의 prelude/스칼라-leaf 방출을 `scalars`로 분기하면 된다.

> **`maude_theory`의 새 역할 (native_system 제거됨).** 복구하면서 죽은
> `native_system` fold(와 `native_term`/`scalar_pat`/`replaced_rule`/`peano_value`)는
> **삭제**했다 — 위처럼 Native를 `of_spec`가 직접 내므로 fold 소비자가 없다. 남은
> `maude_theory`는 **공유 저수준 모듈**: wrapper 철자(`nat_wrap_sym`/`int_wrap_sym`/
> `bool_wrap_sym`/`text_wrap_sym`)와 리터럴 빌더(`nat_t`/`int_t`/`bool_t`/`text_t`),
> `is_literal_sym`/`string_literal`/`chars_value`만 보유한다. **def/use 불변식의 만남
> 지점**이다 — `of_spec ~scalars:Native`(방출)·`To_maude`(delegation eq + start-term
> 인코더)·`Of_maude`(디코더)가 모두 이 철자/빌더를 써야 한다.

**내장 sort는 `Val` 바깥**에 둠(kind가 병합되면 import된 연산자 attribute가 충돌).
`TextE ""`는 bare `nil`로 두고 `List < Text` + `eq`/`cat` nil-방정식으로 bridge.
(이 sort 위계 결정은 `Native` 경로에 한정 — `Structural`은 자체 스칼라라 무관.)

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

### 6.5 confluence/coherence 게이트 — `Mfe` (Maude Formal Environment; 골격에 유지)

confluence 게이트는 CoCoWeb(웹 POST) **대신 `Mfe`**(Full Maude + CRC + ChC, 로컬
`maude`)로 바뀌었다. `Rewrite_system.string_of_system_maude ~rule_heads`가 분석
시스템(구조적 스칼라)을 **단일 sort 시스템 모듈**로 내는데, 등식 fragment는 `eq`/`ceq`,
`rule_heads`(비입력-moded relation = `To_ctrs.rule_head_syms` = `input_moded_rel_syms`의
여집합)는 `rl`/`crl`로 가른다. `Mfe.check`가 MFE를 로컬 maude에 로드해 **한 invocation**에 두 검사를 돌리고
`{ church_rosser; coherence }`를 돌려준다:
- **CRC** — 등식 fragment의 Church-Rosser. "`reduce`가 well-defined인가" = 등식이 결정적.
- **ChC** — `rl`이 등식에 coherent한가. 등식 환원이 규칙 redex를 숨기지 않아 `search`가
  등식 mod로 완전.
둘은 직교한 well-formedness 조건이고 Maude가 *실행 중엔 검사 안 하고 가정만* 한다 — 그래서
오프라인 게이트로 따로 검증한다. MFE는 repo 미체크인(다운로드·경로 해소·미설치 시 깨끗한
`Error`): [tools/mfe/README.md](../../tools/mfe/README.md).

> **실측 프로토콜 (2026-06, MFE-master + Maude 3.5.1).** 옛 `mfe.ml` 상수는
> 추측이라 틀렸고(아래로 교체). MFE는 `maude FILE`이 아니라 **stdin을 읽는 Full Maude
> 객체 루프**다: entry는 `src/mfe.maude`(`full-maude.maude` 아님), `sload`가 라이브러리를
> 찾도록 maude 바이너리 디렉터리를 **`MAUDE_LIB`로 export**, 그리고 `load mfe.maude` +
> 모듈(첫 줄 `set include BOOL off .`) + 명령을 **stdin으로 파이프**한다. **도구 선택
> 필수**: `(select tool CRC .)`→`(check Church-Rosser SPEC .)`,
> `(select tool ChC .)`→`(check coherence SPEC .)` (선택 없는 bare `(check …)`는 parse
> error). 루프는 **clean quit이 없어** EOF에서 incomplete-input 프롬프트 `> `를 무한
> flood하므로, 브리지는 `--timeout` 데드라인 하에 출력을 읽다가 ChC 출력 뒤 flood가
> 보이면 프로세스를 **kill**하고(정상 종료가 아니라 SIGKILL) 이미 찍힌 verdict를 파싱한다.
> verdict 토큰(공백 정규화 후 substring; MFE가 줄을 터미널 폭에서 wrap): CRC 합류
> `The specification is locally-confluent.`; CRC 미확정 `The following critical pairs
> must be proved joinable:`; ChC coherent `… no rewrite with rules can happen at
> non-overlapping positions of equations left-hand sides.`; verdict 없이 데드라인만
> 지나면 `Timeout`. **전체 시스템 CRC는 critical-pair 폭증으로 안 끝나니
> `verify --symbol NAME`의 per-symbol slice가 실사용 경로** (예: `$lookup` → YES/YES ~1.4s;
> `Run_prog`처럼 전체에 닿는 root는 `TIMEOUT`). 자세히 [tools/mfe/README.md](../../tools/mfe/README.md)
> "Calibration"/"Performance".

> **impty/base end-to-end 결과 (per-symbol).** `$lookup`/`Check_*`/`Eval_expr`/
> `Eval_command` = **CRC YES, ChC YES**; `Eval_prog` = **CRC MAYBE, ChC YES**;
> `Run_prog`(전체-reachable) = **TIMEOUT**. ChC가 전부 YES인 건 impty/base에 `rl`/`crl`이
> 0개(모든 relation input-moded → 등식)라 coherence가 vacuous하기 때문. `Eval_prog`
> 비합류는 진짜다: `Eval-prog(command) = env if Eval-command(nil, command) = env`의
> 결과 `env`가 **전제로만 묶이는 RHS 자유변수**라 CRC가 같은 불투명
> `Eval-command(nil,command)`의 두 증인 `env`/`#env#`를 합류 못 시킨다(ccp SPEC226).
> 상수 RHS인 `Check-prog`(`= true if … = tenv`)는 자유변수가 없어 YES. 실행 표면은 같은
> 전제를 `:=`/`=>` 조건으로 내보내므로 무관 — 분석 표면 join-condition `=` 근사의 발현이지
> 번역 버그가 아니다. 슬라이스 모듈은 `rewrite --ctrs --symbol NAME`으로 덤프.

> **p4 spec도 per-symbol으로 돈다 (sweep).** `verify --list-symbols --sizes`로 슬라이스
> 크기를 한 번에 구해 tractable한 159개(≤200규칙)를 돌린 결과 104 YES / 33 MAYBE / 13
> TIMEOUT / 9 degenerate(규칙 0개), >200규칙 415개는 전체-시스템급이라 미실행. 비합류
> 33건은 **두 분석-표면 원인**으로 갈리며 둘 다 번역 버그가 아니다(실행 표면은
> `:=`/`=>` + owise 보완으로 처리): (1) **전제로만 묶이는 RHS 자유변수**(impty
> `Eval_prog`과 동류 — `$dom_map`의 ccp SPEC4 `set{#K#}=set{K} if $unzip_K(it)=K/=#K#`,
> `$empty_store`, `$ctk_of_typedExpressionIR`), (2) **owise 중첩**(`$is_lpm_key_prime`의
> ccp SPEC1 `false=true if text="lpm"` — Maude CRC가 owise를 임계쌍 생성에서 무시;
> todo.md P2 `otherwise` 예측의 실제 발현, `$lookup`은 가드가 `match_*`라 회피). 자세한
> 표는 [todo.md](todo.md) "Mfe calibration".
>
> **원인 (1)은 `Rewrite_system.fold_premise_binders`로 해소(done).** `ctrs_of_spec`
> 마지막에 전제로만 묶이는 변수를 규칙에 도로 접는다 — 출력 바인더 `(prod,v)`는 rhs로
> 인라인, 순수 접근자 구조분해 `(v,K(..))`는 head 패턴으로 폴드(iteration helper 재귀
> 바인더 포함). 분석 표면 전용(실행 모듈 byte-identical). **수술적**으로 제한: 가드 절
> 폴드는 CRC의 disjointness 가드를 없애 owise 중첩을 노출(YES→MAYBE 역행)하므로 안 함.
> 효과: 무역행, p4의 33 MAYBE 중 **19개 → YES**(출력+접근자, iteration 포함). 남은 14는
> 원인 (2) owise + 일부 multi-clause.

> termination 게이트(`Aprove`/`Muterm`/`Termination` 디스패처: `is_unconditional`이면
> AProVE WST, 아니면 MuTerm; `string_of_system_tpdb` 소비)는 별개 축이며 이 골격에선
> **아직 미복원**이다(이번 작업 범위 밖).

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
  `main.exe rewrite --ctrs specs/impty/base/spec.spectec | diff - spec.ctrs`
  (기본 `rewrite`는 실행 모듈을 내므로 분석-CTRS 골든은 `--ctrs` 경로).
- **M2 — Maude 실행.** `To_ctrs.var_type_hints`, `of_spec`의 `Native` 스칼라 분기(§6.1),
  `To_maude.*`(§6.2–6.3). `Maude_run`로 impty 실행 확인.
- **M3 — 결과-VALUE 오라클.** `Of_maude.*`(§6.4). same-spec interp vs Maude 비교.

[CLAUDE.md]: ./CLAUDE.md
