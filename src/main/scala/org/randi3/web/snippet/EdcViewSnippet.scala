package org.randi3.web.snippet


import scala.xml._
import scala.xml.NodeSeq
import scala.xml.Text

import org.randi3.web.lib.DependencyFactory

import net.liftweb.http.SHtml._
import net.liftweb.http.S._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http._

import net.liftweb.util.Helpers._
import scalaz.NonEmptyList
import org.randi3.web.util.CurrentEDCTrial
import org.randi3.edc.model.openClinica.ConnectionOC


class EdcViewSnippet {


  def view(nodeSeq: NodeSeq): NodeSeq = {
    if (CurrentEDCTrial.isEmpty) S.redirectTo("/edcTrial/listRemote")


    val trial = CurrentEDCTrial.get.get

    bind("edcTrial", nodeSeq,
     "identifier" -> <div>{trial.identifier}</div>,
    "name" -> <div>{trial.name}</div>,
    "description" -> <div>{trial.description}</div>,
    "items" ->  <table class="randi2Table">
      <thead>
        <tr>
          <th>Name</th>
          <th>Description</th>
          <th>Type</th>
          <th>Details</th>
        </tr>
      </thead>
      <tfoot>

      </tfoot>
      <tbody>
        {
        trial.getAllCriteria().flatMap(criterion =>
          <tr>
            <td>{criterion.name}</td>
            <td>{criterion.description}</td>
            <td>{criterion.getClass.getSimpleName}</td>
            <td>{criterion.toString}</td>
          </tr>
        )
        }
      </tbody>
    </table>
    ,
    "importTrial" -> button("import trial", () => redirectTo("/edcTrial/addOrEdit"))
    )
  }


}
