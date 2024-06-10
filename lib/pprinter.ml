
let pp_pattern fmt pat =
  Ppxlib.Pprintast.pattern fmt pat

let pp_expression fmt expr =
  Ppxlib.Pprintast.expression fmt expr

let pp_structure_item fmt stri =
  Ppxlib.Pprintast.structure_item fmt stri

let pp_structure fmt str =
  Ppxlib.Pprintast.structure fmt str

let show_exp = [%show: expression]
let show_pat = [%show: pattern]
let show_strct = [%show: structure]
let show_strcti = [%show: structure_item]
