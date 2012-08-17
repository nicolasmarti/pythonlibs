open Pycaml;;

open Lang_intf;;
open Pylang;;

module PyLisp = PyLang(Lisplang.L);;

pymain Sys.argv;;

