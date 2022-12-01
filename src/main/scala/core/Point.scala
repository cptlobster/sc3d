package org.cptlobster.sc3d
package core

trait Point[T] {
  def toArray: Array[T]
  def toString: String
  def normalize: Point[T]
}
