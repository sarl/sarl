import io.sarl.core.Initialize;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.DynamicSkillProvider;
import io.sarl.lang.core.Event;
import java.util.Collection;
import java.util.Set;
import java.util.UUID;
import javax.inject.Inject;
import org.eclipse.xtext.xbase.lib.InputOutput;

@SarlSpecification("0.12")
@SarlElementType(19)
@SuppressWarnings("all")
public class BootAgent extends Agent {
  private void $behaviorUnit$Initialize$0(final Initialize occurrence) {
    event1 e1 = new event1();
    event2 e2 = new event2();
    event3 e3 = new event3(Integer.valueOf(5));
    event4 e4 = new event4(Integer.valueOf(2));
    e2.subid = 4;
    InputOutput.<Integer>println(Integer.valueOf((e2.id + e2.subid)));
    InputOutput.<Integer>println(Integer.valueOf((((e3.initvalue) == null ? 0 : (e3.initvalue).intValue()) + e3.subid)));
    InputOutput.<Integer>println(Integer.valueOf(((((e4.initvalue) == null ? 0 : (e4.initvalue).intValue()) + e4.subid) + e4.subsubid)));
  }
  
  @SyntheticMember
  @PerceptGuardEvaluator
  private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {
    assert occurrence != null;
    assert ___SARLlocal_runnableCollection != null;
    ___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));
  }
  
  @SyntheticMember
  @Override
  public void $getSupportedEvents(final Set<Class<? extends Event>> toBeFilled) {
    super.$getSupportedEvents(toBeFilled);
    toBeFilled.add(Initialize.class);
  }
  
  @SyntheticMember
  @Override
  public boolean $isSupportedEvent(final Class<? extends Event> event) {
    if (Initialize.class.isAssignableFrom(event)) {
      return true;
    }
    return false;
  }
  
  @SyntheticMember
  @Override
  public void $evaluateBehaviorGuards(final Object event, final Collection<Runnable> callbacks) {
    super.$evaluateBehaviorGuards(event, callbacks);
    if (event instanceof Initialize) {
      final Initialize occurrence = (Initialize) event;
      $guardEvaluator$Initialize(occurrence, callbacks);
    }
  }
  
  @SyntheticMember
  public BootAgent(final UUID parentID, final UUID agentID) {
    super(parentID, agentID);
  }
  
  @SyntheticMember
  @Inject
  public BootAgent(final UUID parentID, final UUID agentID, final DynamicSkillProvider skillProvider) {
    super(parentID, agentID, skillProvider);
  }
}
