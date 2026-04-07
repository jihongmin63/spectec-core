module Refactor = Refactor

type selected_rule = Refactor.selected_rule

let parse = Parse_il_value.parse
let parse_il_value = Parse_il_value.parse_il_value
let parse_rule_selector = Refactor.parse_rule_selector
let find_relation_rule = Refactor.find_relation_rule
let refactor_rule = Refactor.refactor_rule