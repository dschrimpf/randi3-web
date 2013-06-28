package org.randi3.web.snippet

import net.liftweb.util.Helpers._

import xml.{NodeSeq, Group}
import org.randi3.web.util.CurrentTrial
import net.liftweb.http.S


class TrialSelectForm {

  def render(xhtml: Group): NodeSeq = {
    if (CurrentTrial.get.isDefined) {
      bind("trial", xhtml,
        "name" ->  <div id="selectedTrial" >{S.?("header.selectedTrial")}:<br /><b>{CurrentTrial.get.get.name}</b></div>
      )
    } else {
      bind("trial", xhtml,
        "name" -> ""

      )
    }

  }
}
