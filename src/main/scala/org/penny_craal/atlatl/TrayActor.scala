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
import javax.swing.{ImageIcon, JOptionPane, SwingUtilities}

import akka.actor.{Actor, ActorLogging}

import scala.concurrent.Future

case class UpdateToolTip(tooltip: String)
case object Exit
case object Suspend

object TrayActor {
  def isSupported: Boolean = SystemTray.isSupported
}

class TrayActor(private val conf: Config) extends Actor with ActorLogging {
  val trayIconFileName = "atlatl.png"

  if (!TrayActor.isSupported) {
    throw new UnsupportedOperationException("System tray is not supported.")
  }

  private val iconFile = ImageIO.read(getClass.getResource("/" + trayIconFileName))
  private val icon = new ImageIcon(iconFile)
  private val trayIcon = new TrayIcon(iconFile)

  SwingUtilities.invokeLater(() => {
    trayIcon.setPopupMenu(makePopupMenu())
    SystemTray.getSystemTray.add(trayIcon)
    log.info("system tray set up")
  })

  private def makePopupMenu(): PopupMenu = {
    val popup = new PopupMenu()
    val aboutItem = new MenuItem("About")
    val suspendItem = new MenuItem("Suspend")
    val exitItem = new MenuItem("Exit")
    aboutItem.addActionListener((_: ActionEvent) => displayAboutDialog())
    suspendItem.addActionListener((_: ActionEvent) => context.parent ! Suspend)
    exitItem.addActionListener((_: ActionEvent) => context.parent ! Exit)
    popup.add(aboutItem)
    popup.add(suspendItem)
    if (!conf.hideExitMenuItem) {
      popup.addSeparator()
      popup.add(exitItem)
    }
    popup
  }

  private def displayAboutDialog(): Unit = {
    val aboutMessage =
      """Atlatl, The Loud Application Time Limiter
        |Copyright (C) 2016, 2017  Ville Jokela
        |
        |This program is free software: you can redistribute it and/or modify
        |it under the terms of the GNU General Public License as published by
        |the Free Software Foundation, either version 3 of the License, or
        |(at your option) any later version.
        |
        |This program is distributed in the hope that it will be useful,
        |but WITHOUT ANY WARRANTY; without even the implied warranty of
        |MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        |GNU General Public License for more details
        |
        |You should have received a copy of the GNU General Public License
        |along with this program.  If not, see <http://www.gnu.org/licenses/>.
      """.stripMargin
    // JOptionPane.showMessageDialog() blocks until the user clicks ok, so let's not execute it in this thread
    import context.dispatcher
    Future { JOptionPane.showMessageDialog(null, aboutMessage, "About atlatl", JOptionPane.INFORMATION_MESSAGE, icon) }
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