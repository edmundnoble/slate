package dash

package object views {
  type View[-Model, +Tag] = (Model => Tag)
}
