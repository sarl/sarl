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
 
package io.sarl.lang.tests.compilation

import com.google.inject.Inject
import io.sarl.lang.SARLInjectorProvider
import org.eclipse.xtext.junit4.InjectWith
import org.eclipse.xtext.junit4.XtextRunner
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper
import org.junit.Assert
import org.junit.Test
import org.junit.runner.RunWith

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class EventCompilerTest {
	@Inject extension CompilationTestHelper

	@Test
	def basicCompile() {
			'''
			event E1 {
				
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Event;

			@SuppressWarnings("all")
			public class E1 extends Event {
			  /**
			   * Construct an event. The source of the event is unknown.
			   * 
			   */
			  public E1() {
			    super();
			  }
			  
			  /**
			   * Construct an event.
			   * @param source - address of the agent that is emitting this event.
			   * 
			   */
			  public E1(final io.sarl.lang.core.Address source) {
			    super(source);
			  }
			  
			  private final static long serialVersionUID = 588368462L;
			}
		''')
	}
	
	@Test
	def withVarAttributesCompile(){
		'''
		event E1 {
			var name : String
		}
		'''.assertCompilesTo('''
		import io.sarl.lang.core.Event;

		@SuppressWarnings("all")
		public class E1 extends Event {
		  public String name;
		  
		  /**
		   * Construct an event. The source of the event is unknown.
		   * 
		   */
		  public E1() {
		    super();
		  }
		  
		  /**
		   * Construct an event.
		   * @param source - address of the agent that is emitting this event.
		   * 
		   */
		  public E1(final io.sarl.lang.core.Address source) {
		    super(source);
		  }
		  
		  @Override
		  public boolean equals(final Object obj) {
		    if (this == obj)
		      return true;
		    if (obj == null)
		      return false;
		    if (getClass() != obj.getClass())
		      return false;
		    if (!super.equals(obj))
		      return false;
		    E1 other = (E1) obj;
		    if (this.name == null) {
		      if (other.name != null)
		        return false;
		    } else if (!this.name.equals(other.name))
		      return false;
		    return true;
		  }
		  
		  @Override
		  public int hashCode() {
		    final int prime = 31;
		    int result = super.hashCode();
		    result = prime * result + ((this.name== null) ? 0 : this.name.hashCode());
		    return result;
		  }
		  
		  /**
		   * Returns a String representation of the Event E1 attributes only.
		   */
		  protected String attributesToString() {
		    StringBuilder result = new StringBuilder(super.attributesToString());
		    result.append("name  = ").append(this.name);
		    return result.toString();
		  }
		  
		  private final static long serialVersionUID = 591742169L;
		}
		''')
	}
	
	@Test
	def inheritanceCompile(){
		val expectedE2 = '''
		@SuppressWarnings("all")
		public class E2 extends E1 {
		  /**
		   * Construct an event. The source of the event is unknown.
		   * 
		   */
		  public E2() {
		    super();
		  }
		  
		  /**
		   * Construct an event.
		   * @param source - address of the agent that is emitting this event.
		   * 
		   */
		  public E2(final io.sarl.lang.core.Address source) {
		    super(source);
		  }
		  
		  private final static long serialVersionUID = 2189L;
		}
		'''
		
		'''
			event E1 {
				var name : String
			}
			
			event E2 extends E1{
			}
		'''.compile([CompilationTestHelper.Result r |
			Assert.assertEquals(expectedE2,r.getGeneratedCode("E2"))
		]);
	}
	
	@Test
	def staticField() {
			'''
			event E1 {
				val titi : int = 4
				val toto : int
				new(a : int) {
					this.toto = a
				}
			}
		'''.assertCompilesTo('''
			import io.sarl.lang.core.Event;

			@SuppressWarnings("all")
			public class E1 extends Event {
			  public final static int titi = 4;
			  
			  public final int toto;
			  
			  public E1(final int a) {
			    this.toto = a;
			  }
			  
			  @Override
			  public boolean equals(final Object obj) {
			    if (this == obj)
			      return true;
			    if (obj == null)
			      return false;
			    if (getClass() != obj.getClass())
			      return false;
			    if (!super.equals(obj))
			      return false;
			    E1 other = (E1) obj;
			    if (other.titi != this.titi)
			      return false;
			    if (other.toto != this.toto)
			      return false;
			    return true;
			  }
			  
			  @Override
			  public int hashCode() {
			    final int prime = 31;
			    int result = super.hashCode();
			    result = prime * result + this.titi;
			    result = prime * result + this.toto;
			    return result;
			  }
			  
			  /**
			   * Returns a String representation of the Event E1 attributes only.
			   */
			  protected String attributesToString() {
			    StringBuilder result = new StringBuilder(super.attributesToString());
			    result.append("titi  = ").append(this.titi);
			    result.append("toto  = ").append(this.toto);
			    return result.toString();
			  }
			  
			  private final static long serialVersionUID = 595497177L;
			}
		''')
	}
}