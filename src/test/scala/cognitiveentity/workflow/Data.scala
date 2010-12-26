/* Copyright (c) 2010 Richard Searle
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * @author Richard Searle
 */
package cognitiveentity.workflow

case class Num(s:String)

case class Acct(s:String)

trait BalanceLike{
 def v:Float
}

case class Bal(v:Float) extends BalanceLike{
    def +(o:Bal) = Bal(v+o.v)
}

case class PP(v:Float) extends BalanceLike