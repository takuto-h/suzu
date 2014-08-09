
let str = "\nmodule List:\n  class C:\n    def Nil()\n    def Cons(x, xs)\n  end\n\n  let Nil = Nil()\n\n  def append(lst1, lst2):\n    match(lst1):\n    case([]):\n      lst2\n    case([x | xs]):\n      Cons(x, append(xs, lst2))\n    end\n  end\n\n  module Open:\n    let C#(++) = append\n\n    def C#to_string(self):\n      match(self):\n      case([]):\n        \"[]\"\n      case([x | xs]):\n        String::format(\"{0}::{1}\", x, xs)\n      end\n    end\n\n    export C#(++), C#to_string\n  end\n\n  export C, Nil, Cons, append, Open\nend\n\nopen List::Open\n"

let initialize interp =
  Interp.parse_string interp "modList.sn" str
