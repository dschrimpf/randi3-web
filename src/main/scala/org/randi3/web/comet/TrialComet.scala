package org.randi3.web.comet

import net.liftweb.http.CometActor
import org.randi3.web.util.CurrentTrial

class TrialComet extends CometActor {
  
  private var actualTrial = CurrentTrial.get

  def render = { 
    <div>test</div>
  }

}
