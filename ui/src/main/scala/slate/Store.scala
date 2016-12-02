package slate

import slate.util.ExternalVar

final class Store[S](var state: S){
  def view: ExternalVar[S] =
    ExternalVar[S](() => state, s => state = s)
}

