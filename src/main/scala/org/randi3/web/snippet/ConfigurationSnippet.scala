package org.randi3.web.snippet

import xml.NodeSeq
import org.randi3.web.util.CurrentUser
import net.liftweb.http.S._
import org.randi3.web.lib.DependencyFactory

class ConfigurationSnippet {
  def configured(html: NodeSeq) =
    if (DependencyFactory.configurationService.isConfigurationComplete) html else NodeSeq.Empty

  def install(html: NodeSeq) =
    if (!DependencyFactory.configurationService.isConfigurationComplete) redirectTo("install") else NodeSeq.Empty
}
