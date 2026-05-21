open Lang.Il

type ir_var = {
  iv_id  : id';
  iv_typ : typ;
}

type qc_prems = {
  qp_inputs  : id' list;          (* names of variables passed as inputs *)
  qp_outputs : (id' * typ) list;  (* names+types of variables returned as outputs *)
  qp_prems   : prem list;         (* elaborated IL premises *)
}

type qc_command =
  | QcProp of {
      name       : string;
      free_vars  : ir_var list;
      generator  : string option;
      generalize : bool;
      prems      : qc_prems;
      goal       : prem;
    }
  | QcGen of {
      name      : string;
      free_vars : ir_var list;
      generator : string option;
      prems     : qc_prems;
    }

type t = qc_command list
