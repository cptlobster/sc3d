package org.cptlobster.sc3d
package core

trait TypeSafeNum {
  def + (n: TypeSafeNum): TypeSafeNum
  def - (n: TypeSafeNum): TypeSafeNum
  def * (n: TypeSafeNum): TypeSafeNum
  def / (n: TypeSafeNum): TypeSafeNum
  def as[T]: T
}
