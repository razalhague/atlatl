/* Copyright © 2016 Ville Jokela
 *
 * This file is part of atlatl.
 *
 * atlatl is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * atlatl is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with atlatl.  If not, see <http://www.gnu.org/licenses/>.
 *
 * contact me <ville.jokela@penny-craal.org>
 */

package org.penny_craal.atlatl

import java.io.{PrintWriter, StringWriter}
import javax.swing.JOptionPane

import akka.actor.SupervisorStrategy.Escalate
import akka.actor.{OneForOneStrategy, SupervisorStrategy, SupervisorStrategyConfigurator}

/**
  * @author Ville Jokela
  */
class AtlatlSsc extends SupervisorStrategyConfigurator {
  override def create(): SupervisorStrategy = OneForOneStrategy() {
    case e: Exception =>
      val sw = new StringWriter()
      e.printStackTrace(new PrintWriter(sw))
      JOptionPane.showMessageDialog(null, sw.toString, "Error", JOptionPane.ERROR_MESSAGE)
      Escalate
  }
}
