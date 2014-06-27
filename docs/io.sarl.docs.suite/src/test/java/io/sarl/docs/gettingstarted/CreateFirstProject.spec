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
	 * Fill the elements in the project creation wizard.
	 * 
	 * Open the file `pom.xml`, and put the following configuration inside.
	 * 
	 *     <project>
	 *        ...
	 *        <properties>
	 *           ...
	 *           <sarl.version>0.1.0</sarl.version>
	 *           <janus.version>2.0.1.0</janus.version>
	 *        </properties>
	 *        ...
	 *        <build>
	 *           <plugins>
	 *              ...
	 *              <plugin>
	 *                 <groupId>org.codehaus.mojo</groupId>
	 *                 <artifactId>build-helper-maven-plugin</artifactId>
	 *                 <executions>
	 *                    <execution>
	 *                       <goals>
	 *                          <goal>add-source</goal>
	 *                       </goals>
	 *                       <configuration>
	 *                          <sources>
	 *                             <source>src/main/sarl</source>
	 *                             <source>src/main/generated-sources/xtend/</source>
	 *                             <source>src/test/generated-sources/xtend/</source>
	 *                          </sources>
	 *                       </configuration>
	 *                    </execution>
	 *                 </executions>
	 *              </plugin>
	 *              <plugin>
	 *                 <groupId>org.apache.maven.plugins</groupId>
	 *                 <artifactId>maven-clean-plugin</artifactId>
	 *                 <executions>
	 *                    <execution>
	 *                       <phase>clean</phase>
	 *                       <goals>
	 *                          <goal>clean</goal>
	 *                       </goals>
	 *                       <configuration>
	 *                          <filesets>
	 *                             <fileset>
	 *                                <directory>src/main/generated-sources/xtend</directory>
	 *                             </fileset>
	 *                             <fileset>
	 *                                <directory>src/test/generated-sources/xtend</directory>
	 *                             </fileset>
	 *                          </filesets>
	 *                       </configuration>
	 *                    </execution>
	 *                 </executions>
	 *              </plugin>
	 *              <plugin>
	 *                 <groupId>org.eclipse.xtext</groupId>
	 *                 <artifactId>xtext-maven-plugin</artifactId>
	 *                 <executions>
	 *                    <execution>
	 *                       <goals>
	 *                          <goal>generate</goal>
	 *                       </goals>
	 *                    </execution>
	 *                 </executions>
	 *                 <configuration>
	 *                    <compilerSourceLevel>1.7</compilerSourceLevel>
	 *                    <compilerTargetLevel>1.7</compilerTargetLevel>
	 *                    <encoding>UTF-8</encoding>
	 *                    <languages>
	 *                       <language>
	 *                          <setup>io.sarl.lang.SARLStandaloneSetup</setup>
	 *                          <outputConfigurations>
	 *                             <outputConfiguration>
	 *                                <outputDirectory>src/main/generated-sources/xtend/</outputDirectory>
	 *                             </outputConfiguration>
	 *                          </outputConfigurations>
	 *                       </language>
	 *                    </languages>
	 *                 </configuration>
	 *                 <dependencies>
	 *                    <dependency>
	 *                       <groupId>io.sarl.lang</groupId>
	 *                       <artifactId>io.sarl.lang</artifactId>
	 *                       <version>${sarl.version}</version>
	 *                    </dependency>
	 *                    <dependency>
	 *                       <groupId>io.sarl.lang</groupId>
	 *                       <artifactId>io.sarl.lang.core</artifactId>
	 *                       <version>${sarl.version}</version>
	 *                    </dependency>
	 *                 </dependencies>
	 *              </plugin>
	 *           </plugins>
	 *        </build>
	 *        ...
	 *        <dependencies>
	 *           ...
	 *           <dependency>
	 *              <groupId>io.janusproject</groupId>
	 *              <artifactId>io.janusproject.kernel</artifactId>
	 *              <version>${janus.version}</version>
	 *           </dependency>
	 *           ...
	 *        </dependencies>
	 *        ...
	 *        <repositories>
	 *           ...
	 *           <repository>
	 *              <id>sarl-repository</id>
	 *              <url>http://maven.sarl.io/</url>
	 *           </repository>
	 *        </repositories>
	 *        ...
	 *     </project>
	 * 
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
	 * In the next section, we will learn how to create our first agent.
	 * 
	 * [Next](AgentDefinitionIntroductionSpec.html).
	 */
	describe "What's next?" { }

}
