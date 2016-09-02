package de

import cats.kernel.instances.ListInstances
import cats.instances.FutureInstances

package object codecentric extends CatsPkg

trait CatsPkg extends ListInstances with FutureInstances
