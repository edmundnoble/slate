package slate.bindings

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.typedarray.Uint8Array

@js.native
@JSName("LZString")
object LZString extends js.Any {
  @js.native
  def compressToUint8Array(str: String): Uint8Array = js.native

  @js.native
  def decompressFromUint8Array(buf: Uint8Array): String = js.native

  @js.native
  def compressToUTF16(str: String): String = js.native

  @js.native
  def decompressFromUTF16(str: String): String = js.native

  @js.native
  def compress(str: String): String = js.native

  @js.native
  def decompress(str: String): String = js.native
}
