open Env

(* Environments *)

(* Identifier type and dimension environment *)

module VEnv = MakeIdMap (Typ)

(* Plain type (EL type) environment *)

module PTEnv = MakeIdMap (Plaintyp)

(* Type definition environment *)

module TDEnv = MakeTIdMap (Typdef)

(* Relation environment *)

module HEnv = MakeIdMap (Rel.Hint)
module REnv = MakeRIdMap (Rel)

(* Definition environment *)

module FEnv = MakeFIdMap (Func)
