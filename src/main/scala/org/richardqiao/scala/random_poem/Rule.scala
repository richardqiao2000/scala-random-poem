package org.richardqiao.scala.random_poem

import scala.util._
import java.io._
import scala.collection.mutable._

object Rule{
  val map: Map[String, Rule] = new HashMap[String, Rule]()
  var props: java.util.Properties = null
  
  def main(args: Array[String]): Unit = {
    println(Rule("POEM").randomGen)
  }
  
  def apply(rule: String): Rule = {
    new Rule(rule)
  }
}

class Rule {
  var isWordList: Boolean = false
  var words: Array[String] = null
  var rules: Array[Array[Rule]] = null
  
  def this(wds: Array[String]) = {
    this()
    isWordList = true
    words = wds
  }
  
  def this(ruleName: String){
    this()
    if(Rule.props == null){
      Rule.props = new java.util.Properties()
      Rule.props.load(new FileInputStream("src/main/resources/rules.properties"))
    }
    val rule = Rule.props.getProperty(ruleName)
    if(rule != null){
      Rule.map += (ruleName -> this)
      rules = rule.split(' ').map(str => {
        val tmp = str.trim
        if(tmp.startsWith("$") || tmp.startsWith("<")){
          val tmp2 = tmp.replace("<", "").replace(">", "")
          getRules(tmp2)
        }else{
          getWordRules(tmp)
        }
      })
      
    }
  }
  
  def getRules(rule: String): Array[Rule] = {
    rule.split("\\|").map(str => {
      val tmp = str.trim
      if(Rule.map.contains(tmp)){
        Rule.map(tmp)
      }else{
        if(tmp.equals("$END")){
          new Rule()
        }else if(tmp.equals("$LINEBREAK")){
          val rl = new Rule(Array[String]{"\n"})
          rl.isWordList = true
          rl
        }else{
          new Rule(tmp)
        }
      }
    })
  }
  
  def getWordRules(rule: String): Array[Rule] = {
    Array[Rule]{new Rule(rule.split("\\|"))}
  }
  
  def getRandomWord(): String = {
    words(new Random().nextInt(words.length))
  }
  
  def getRandomRule(rules: Array[Rule]): Rule = {
    rules(new Random().nextInt(rules.length))
  }
  
  def randomGen(): String = {
    if(rules == null && words == null) return ""
    val sb = new StringBuilder()
    rules.foreach(rs => {
      val rule = getRandomRule(rs)
      var tmp: String = ""
      if(rule.isWordList){
        tmp = rule.getRandomWord()
        tmp = tmp.substring(0, 1).toUpperCase() + tmp.substring(1, tmp.length())
      }else{
        tmp = rule.randomGen()
      }
      if(tmp.trim().length() > 0
          && sb.length() > 0
          && sb.charAt(sb.length() - 1) != '\n'){
        sb.append(" ")
      }
      sb.append(tmp)
    })
    sb.toString();
  }

}
