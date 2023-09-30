module Myo.Command.Edit.Syntax where

import Exon (exon)
import Ribosome.Syntax (Syntax, build, link, match, prefix, (#>), (>-), (<#>))

editSyntax :: Syntax
editSyntax =
  build $
  prefix "MyoEdit" $
  match "Line" "^.*$" #> (param <#> cmdline) >- link "Constant" (match "Value" [exon|\S.*$|])
  where
    param =
      link "Type" (match "ParamId" [exon| 🛠 \S\+:|])
    cmdline =
      link "Directory" (match "CmdlineNo" [exon| 🟢 \S\+:|])
