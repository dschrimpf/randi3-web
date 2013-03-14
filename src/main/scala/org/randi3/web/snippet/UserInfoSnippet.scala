package org.randi3.web.snippet

import xml.NodeSeq

import net.liftweb.http.S._

class UserInfoSnippet {

  def info(nodeSeq: NodeSeq): NodeSeq = {
    redirectTo("/user/list")
  }
}
