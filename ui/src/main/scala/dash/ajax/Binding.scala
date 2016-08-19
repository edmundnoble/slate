package dash.ajax

import shapeless.HList
import shapeless.ops.record.ToMap

case class Binding[PathTy <: PathSegment, DataRec <: HList, Headers <: HList]
(path: PathTy, method: AjaxMethod)
(implicit val dataToMap: ToMap.Aux[DataRec, String, Any], val headersToMap: ToMap.Aux[Headers, String, String])
