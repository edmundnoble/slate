package dash.ajax

import shapeless.HList
import shapeless.ops.record.ToMap

case class Binding[PathTy <: PathSegment, QueryParams <: HList, Headers <: HList]
(path: PathTy, method: AjaxMethod)
(implicit val queryParamsToMap: ToMap.Aux[QueryParams, String, Any], val headersToMap: ToMap.Aux[Headers, String, String])
