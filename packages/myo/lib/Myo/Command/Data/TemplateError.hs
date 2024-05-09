module Myo.Command.Data.TemplateError where

import Data.Some (Some (Some))
import Exon (exon)
import Log (Severity (Error))
import Ribosome (Report (Report), Reportable (..), RpcError, rpcError)

import Myo.Command.Data.Param (ParamId, ParamTag, paramTagName, paramTagType)

data TemplateError =
  NoParamValue ParamId Text Text
  |
  BadParamValue Text (Some ParamTag) (Maybe RpcError)
  |
  ParamFunError (Some ParamTag) RpcError
  |
  DynamicParseError (Some ParamTag) Text
  |
  Optparse Text
  |
  Internal Text
  deriving stock (Show)

instance Reportable TemplateError where
  toReport (NoParamValue pid var fun) =
    Report [exon|Neither 'g:#{var}' nor '#{fun}()' exists for command parameter '##{pid}' without default|] log Error
    where
      log = ["RunError.NoParamValue:", show pid, var, fun]

  toReport (BadParamValue provenance (Some ptag) err) =
    Report [exon|The #{provenance} for command parameter '#{name}' is not a #{tpe}|] log Error
    where
      log = ["RunError.BadParamValue:", name] <> foldMap (pure . rpcError) err
      tpe = paramTagType ptag
      name = paramTagName ptag

  toReport (ParamFunError (Some ptag) err) =
    Report [exon|The function for command parameter '#{name}' failed: #{rpcError err}|] log Error
    where
      log = ["RunError.ParamFunError:", name, rpcError err]
      name = paramTagName ptag

  toReport (DynamicParseError (Some ptag) err) =
    Report [exon|Parse error in parameter '#{name}'. If it is no template, please escape braces.|] log Error
    where
      log = ["RunError.DynamicTemplateParseError:", name, err]
      name = paramTagName ptag

  toReport (Optparse err) =
    Report err log Error
    where
      log = ["RunError.Optparse:", err]

  toReport (Internal e) =
    Report [exon|Internal error: #{e}|] ["TemplateError.Internal:", e] Error
