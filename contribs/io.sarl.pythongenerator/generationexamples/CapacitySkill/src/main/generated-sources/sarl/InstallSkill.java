import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Skill;

@SarlSpecification("0.12")
@SarlElementType(22)
@SuppressWarnings("all")
public class InstallSkill extends Skill implements Logging {
  public void info(final String text) {
  }
  
  public void debug(final String text) {
  }
  
  public void install() {
    class $AssertEvaluator$ {
      final boolean $$result;
      $AssertEvaluator$() {
        Agent _owner = InstallSkill.this.getOwner();
        this.$$result = (_owner != null);
      }
    }
    assert new $AssertEvaluator$().$$result;
  }
  
  public void uninstall() {
  }
  
  @SyntheticMember
  public InstallSkill() {
    super();
  }
  
  @SyntheticMember
  public InstallSkill(final Agent agent) {
    super(agent);
  }
}
