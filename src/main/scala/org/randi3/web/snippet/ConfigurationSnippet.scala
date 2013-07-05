package org.randi3.web.snippet

import xml.NodeSeq
import org.randi3.web.util.CurrentLoggedInUser
import net.liftweb.http.S._
import org.randi3.web.lib.DependencyFactory

class ConfigurationSnippet {
  def configured(html: NodeSeq) =
    if (DependencyFactory.get.configurationService.isConfigurationComplete) html else NodeSeq.Empty

  def install(html: NodeSeq) =
    if (!DependencyFactory.get.configurationService.isConfigurationComplete) redirectTo("/installation/serverURL") else NodeSeq.Empty
}
