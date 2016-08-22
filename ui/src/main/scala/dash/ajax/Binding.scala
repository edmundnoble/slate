package dash.ajax

import shapeless.HList
import shapeless.ops.record.ToMap

case class Binding[PathTy <: PathSegment, QueryData <: HList, Headers <: HList]
(path: PathTy, method: AjaxMethod)
(implicit val queryDataToMap: ToMap.Aux[QueryData, String, Any], val headersToMap: ToMap.Aux[Headers, String, String])
