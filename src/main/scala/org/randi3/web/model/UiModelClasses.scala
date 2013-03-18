package org.randi3.web.model

import collection.mutable.{HashSet, ListBuffer}
import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint
import org.randi3.randomization.configuration._
import org.randi3.randomization.configuration.BooleanConfigurationType
import org.randi3.randomization.configuration.DoubleConfigurationType
import org.randi3.randomization.configuration.OrdinalConfigurationType
import org.randi3.randomization.configuration.ConfigurationProperty
import org.randi3.randomization.configuration.IntegerConfigurationType


case class TreatmentArmTmp(id: Int, version: Int, var name: String, var description: String, var plannedSize: Int) {}

case class CriterionTmp(id: Int, version: Int, typ: String, var name: String, var description: String, values: Option[ListBuffer[String]], var inclusionConstraint: Option[ConstraintTmp] = None, var strata: ListBuffer[ConstraintTmp] = new ListBuffer()) {}

case class ConstraintTmp(id: Int = Int.MinValue, version: Int = 0, var minValue: Option[String] = None, var maxValue: Option[String] = None, ordinalValues: HashSet[(Boolean, String)] = new HashSet())

case class SubjectDataTmp(criterion: Criterion[Any, Constraint[Any]], var value: Any) {}

case class RandomizationMethodConfigTmp(id: Int = Int.MinValue, version: Int = 0, name: String, description: String, canBeUsedWithStratification: Boolean, configurationEntries: List[RandomizationMethodConfigEntryTmp[Any]]) {

  def getConfigurationProperties: List[ConfigurationProperty[Any]] = {
    configurationEntries.map(config => {
      if (config.configurationType.getClass == classOf[BooleanConfigurationType]) {
        new ConfigurationProperty(config.configurationType, config.value.toString.toBoolean)
      } else if (config.configurationType.getClass == classOf[DoubleConfigurationType]) {
        new ConfigurationProperty(config.configurationType, config.value.toString.toDouble)
      } else if (config.configurationType.getClass == classOf[IntegerConfigurationType]) {
        new ConfigurationProperty(config.configurationType, config.value.toString.toInt)
      } else if (config.configurationType.getClass == classOf[OrdinalConfigurationType]) {
        new ConfigurationProperty(config.configurationType, config.value)
      }
    }).asInstanceOf[List[ConfigurationProperty[Any]]]
  }
}

case class RandomizationMethodConfigEntryTmp[T](configurationType: ConfigurationType[T], var value: T) {}