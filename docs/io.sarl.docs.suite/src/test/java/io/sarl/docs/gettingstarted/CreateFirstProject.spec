/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.docs.gettingstarted

import io.sarl.docs.utils.SARLSpecCreator
import org.jnario.runner.CreateWith

/* For developing with SARL, you should create a project.
 * This document describes two ways for created SARL projects
 * in Eclipse: a SARL standard project, and a Maven project. 
 */
@CreateWith(SARLSpecCreator)
describe "Create First Project" {
	
	/* For creating a project, you should open your Eclipse and click on
	 * **File > New > Others > SARL > Project**.
	 */
	describe "Creating a SARL Project" {
		
	} 

	/* For creating a Maven project with SARL, you should open your 
	 * Eclipse and click on
	 * **File > New > Others > Maven > Maven Project**.
	 * 
	 * Fill the elements in the proejct creation wizard.
	 * 
	 * Open the file `pom.xml`, and put the following configuration inside.
	 * 
	 * <pre><code>
	 * &lt;project&gt;
	 *    ...
	 *    &lt;properties&gt;
	 *       ...
	 *       &lt;sarl.version&gt;0.1.0&lt;/sarl.version&gt;
	 *       &lt;janus.version&gt;2.0.1.0&lt;/janus.version&gt;
	 *    &lt;/properties&gt;
	 *    ...
	 *    &lt;build&gt;
	 *       &lt;plugins&gt;
	 *          ...
	 *          &lt;plugin&gt;
	 *             &lt;groupId&gt;org.codehaus.mojo&lt;/groupId&gt;
	 *             &lt;artifactId&gt;build-helper-maven-plugin&lt;/artifactId&gt;
	 *             &lt;executions&gt;
	 *                &lt;execution&gt;
	 *                   &lt;goals&gt;
	 *                      &lt;goal&gt;add-source&lt;/goal&gt;
	 *                   &lt;/goals&gt;
	 *                   &lt;configuration&gt;
	 *                      &lt;sources&gt;
	 *                         &lt;source&gt;src/main/sarl&lt;/source&gt;
	 *                         &lt;source&gt;src/main/generated-sources/xtend/&lt;/source&gt;
	 *                         &lt;source&gt;src/test/generated-sources/xtend/&lt;/source&gt;
	 *                      &lt;/sources&gt;
	 *                   &lt;/configuration&gt;
	 *                &lt;/execution&gt;
	 *             &lt;/executions&gt;
	 *          &lt;/plugin&gt;
	 *          &lt;plugin&gt;
	 *             &lt;groupId&gt;org.apache.maven.plugins&lt;/groupId&gt;
	 *             &lt;artifactId&gt;maven-clean-plugin&lt;/artifactId&gt;
	 *             &lt;executions&gt;
	 *                &lt;execution&gt;
	 *                   &lt;phase&gt;clean&lt;/phase&gt;
	 *                   &lt;goals&gt;
	 *                      &lt;goal&gt;clean&lt;/goal&gt;
	 *                   &lt;/goals&gt;
	 *                   &lt;configuration&gt;
	 *                      &lt;filesets&gt;
	 *                         &lt;fileset&gt;
	 *                            &lt;directory&gt;src/main/generated-sources/xtend&lt;/directory&gt;
	 *                         &lt;/fileset&gt;
	 *                         &lt;fileset&gt;
	 *                            &lt;directory&gt;src/test/generated-sources/xtend&lt;/directory&gt;
	 *                         &lt;/fileset&gt;
	 *                      &lt;/filesets&gt;
	 *                   &lt;/configuration&gt;
	 *                &lt;/execution&gt;
	 *             &lt;/executions&gt;
	 *          &lt;/plugin&gt;
	 *          &lt;plugin&gt;
	 *             &lt;groupId&gt;org.eclipse.xtext&lt;/groupId&gt;
	 *             &lt;artifactId&gt;xtext-maven-plugin&lt;/artifactId&gt;
	 *             &lt;executions&gt;
	 *                &lt;execution&gt;
	 *                   &lt;goals&gt;
	 *                      &lt;goal&gt;generate&lt;/goal&gt;
	 *                   &lt;/goals&gt;
	 *                &lt;/execution&gt;
	 *             &lt;/executions&gt;
	 *             &lt;configuration&gt;
	 *                &lt;compilerSourceLevel&gt;1.7&lt;/compilerSourceLevel&gt;
	 *                &lt;compilerTargetLevel&gt;1.7&lt;/compilerTargetLevel&gt;
	 *                &lt;encoding&gt;UTF-8&lt;/encoding&gt;
	 *                &lt;languages&gt;
	 *                   &lt;language&gt;
	 *                      &lt;setup&gt;io.sarl.lang.SARLStandaloneSetup&lt;/setup&gt;
	 *                      &lt;outputConfigurations&gt;
	 *                         &lt;outputConfiguration&gt;
	 *                            &lt;outputDirectory&gt;src/main/generated-sources/xtend/&lt;/outputDirectory&gt;
	 *                         &lt;/outputConfiguration&gt;
	 *                      &lt;/outputConfigurations&gt;
	 *                   &lt;/language&gt;
	 *                &lt;/languages&gt;
	 *             &lt;/configuration&gt;
	 *             &lt;dependencies&gt;
	 *                &lt;dependency&gt;
	 *                   &lt;groupId&gt;io.sarl.lang&lt;/groupId&gt;
	 *                   &lt;artifactId&gt;io.sarl.lang&lt;/artifactId&gt;
	 *                   &lt;version&gt;${sarl.version}&lt;/version&gt;
	 *                &lt;/dependency&gt;
	 *                &lt;dependency&gt;
	 *                   &lt;groupId&gt;io.sarl.lang&lt;/groupId&gt;
	 *                   &lt;artifactId&gt;io.sarl.lang.core&lt;/artifactId&gt;
	 *                   &lt;version&gt;${sarl.version}&lt;/version&gt;
	 *                &lt;/dependency&gt;
	 *             &lt;/dependencies&gt;
	 *          &lt;/plugin&gt;
	 *       &lt;/plugins&gt;
	 *    &lt;/build&gt;
	 *    ...
	 *    &lt;dependencies&gt;
	 *       ...
	 *       &lt;dependency&gt;
	 *          &lt;groupId&gt;io.janusproject&lt;/groupId&gt;
	 *          &lt;artifactId&gt;io.janusproject.kernel&lt;/artifactId&gt;
	 *          &lt;version&gt;${janus.version}&lt;/version&gt;
	 *       &lt;/dependency&gt;
	 *       ...
	 *    &lt;/dependencies&gt;
	 *    ...
	 *    &lt;repositories&gt;
	 *       ...
	 *       &lt;repository&gt;
	 *          &lt;id&gt;sarl-repository&lt;/id&gt;
	 *          &lt;url&gt;http://maven.sarl.io/&lt;/url&gt;
	 *       &lt;/repository&gt;
	 *    &lt;/repositories&gt;
	 *    ...
	 * &lt;/project&gt;
	 * </code></pre> 
	 * 
	 * Replace the version number (`0.1.0`) of SARL
	 * with the one you want to use.
	 * 
	 * Replace the version number (`2.0.1.0`) of the [Janus platform](http://www.janusproject.io)
	 * with the one you want to use.
	 */
	describe "Creating a Maven Project" {
		
	} 
	
	/*
	 * In the next section, we will learn how to create your first agent.
	 * 
	 * [Next](AgentDefinitionIntroductionSpec.html).
	 */
	describe "What's next?" { }

}
