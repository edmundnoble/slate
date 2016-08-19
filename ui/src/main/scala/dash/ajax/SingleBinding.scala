package dash.ajax

import shapeless.HList
import shapeless.ops.record.ToMap

class SingleBinding[PathTy <: PathSegment, DataRec <: HList, Headers <: HList]
(val path: PathTy, val method: AjaxMethod)
(implicit val dataToMap: ToMap.Aux[DataRec, String, Any], val headersToMap: ToMap.Aux[Headers, String, String])
