package slate.app.builtin

import qq.macros.stager.QQStager

import scala.reflect.macros.whitebox

class SlateStager(c: whitebox.Context) extends QQStager(c)(SlatePrelude) {

}
