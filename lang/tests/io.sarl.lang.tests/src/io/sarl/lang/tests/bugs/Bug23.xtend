package io.sarl.lang.tests.bugs

import org.eclipse.xtext.junit4.XtextRunner
import org.junit.runner.RunWith
import org.eclipse.xtext.junit4.InjectWith
import io.sarl.lang.SARLInjectorProvider
import com.google.inject.Inject
import org.eclipse.xtext.junit4.util.ParseHelper
import io.sarl.lang.sarl.Model
import org.eclipse.xtext.junit4.validation.ValidationTestHelper
import org.junit.Test
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper
import org.junit.Assert

/**
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner)
@InjectWith(SARLInjectorProvider)
class Bug23 {
	
	@Inject extension ParseHelper<Model>
	@Inject extension ValidationTestHelper
    @Inject extension CompilationTestHelper
	
	val CharSequence snippet = '''
		import java.util.UUID
		event AgentSpawned {
			var agentID : UUID
		}
		
		event MyAgentSpawned extends AgentSpawned{
			var titi : UUID
		}
		
		agent MyAgent {
			on MyAgentSpawned {
				occurrence.titi
				occurrence.agentID
			}
		}
	'''
	
	
	@Test
	def bug23(){
		var mas = snippet.parse
		mas.assertNoErrors
	}
	
	@Test
	def myAgentSpawnedCompile() {
		val expectedMyAgentSpawned = '''
		import java.util.UUID;

		@SuppressWarnings("all")
		public class MyAgentSpawned extends AgentSpawned {
		  private UUID titi;
		  
		  public UUID getTiti() {
		    return this.titi;
		  }
		  
		  public void setTiti(final UUID titi) {
		    this.titi = titi;
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
		    MyAgentSpawned other = (MyAgentSpawned) obj;
		    if (titi == null) {
		      if (other.titi != null)
		        return false;
		    } else if (!titi.equals(other.titi))
		      return false;
		    return true;
		  }
		  
		  @Override
		  public int hashCode() {
		    final int prime = 31;
		    int result = super.hashCode();
		    result = prime * result + ((titi== null) ? 0 : titi.hashCode());
		    return result;
		  }
		  
		  /**
		   * Returns a String representation of the Event MyAgentSpawned attributes only.
		   */
		  protected String attributesToString() {
		    StringBuilder result = new StringBuilder(super.attributesToString());
		    result.append("titi  = ").append(this.titi);
		    return result.toString();
		  }
		  
		  /**
		   * Returns a String representation of the Event MyAgentSpawned.
		   */
		  public String toString() {
		    StringBuilder result = new StringBuilder();
		    result.append("MyAgentSpawned[");
		    result.append(attributesToString());
		    result.append("]");
		    return result.toString();
		  }
		}
		'''
		
		snippet.compile([CompilationTestHelper.Result r |
			Assert.assertEquals(expectedMyAgentSpawned,r.getGeneratedCode("MyAgentSpawned"))
		]);
	}
}