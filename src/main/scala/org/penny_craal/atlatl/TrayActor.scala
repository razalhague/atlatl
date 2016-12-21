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

import java.awt.event.ActionEvent
import java.awt.{MenuItem, PopupMenu, SystemTray, TrayIcon}
import javax.imageio.ImageIO
import javax.swing.SwingUtilities

import akka.actor.{Actor, ActorLogging}

case class UpdateToolTip(tooltip: String)
case object Exit
case object Suspend

class TrayActor extends Actor with ActorLogging {
  val trayIconFileName = "atlatl.png"

  if (!SystemTray.isSupported) {
    throw new UnsupportedOperationException("System tray is not supported.")
  }

  private val trayIcon: TrayIcon = new TrayIcon(ImageIO.read(getClass.getResource("/" + trayIconFileName)))

  SwingUtilities.invokeLater(() => {
    trayIcon.setPopupMenu(makePopupMenu())
    SystemTray.getSystemTray.add(trayIcon)
    log.info("system tray set up")
  })

  def makePopupMenu(): PopupMenu = {
    val popup = new PopupMenu()
    val suspendItem = new MenuItem("Suspend")
    val exitItem = new MenuItem("Exit")
    suspendItem.addActionListener((_: ActionEvent) => context.parent ! Suspend)
    exitItem.addActionListener((_: ActionEvent) => context.parent ! Exit)
    popup.add(suspendItem)
    popup.addSeparator()
    popup.add(exitItem)
    popup
  }

  override def receive: Receive = {
    case UpdateToolTip(tooltip) =>
      trayIcon.setToolTip(tooltip)
      log.info("set tray tooltip to: " + tooltip)
  }

  override def postStop(): Unit = {
    SystemTray.getSystemTray.remove(trayIcon)
  }
}