import io.sarl.core.Behaviors;
import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.DefaultValueSource;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.Skill;
import io.sarl.lang.util.ConcurrentCollection;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Pure;

@SarlSpecification("0.12")
@SarlElementType(22)
@SuppressWarnings("all")
public class FilteringEventDispatchingBehavior extends Skill implements Behaviors {
  public final Class<? extends Event> acceptedType;
  
  public final Behaviors behaviorDelegate;
  
  public FilteringEventDispatchingBehavior(final Class<? extends Event> acceptedType, final Behaviors behaviorDelegate) {
    this.acceptedType = acceptedType;
    this.behaviorDelegate = behaviorDelegate;
  }
  
  @Pure
  @Override
  public EventListener asEventListener() {
    return new FilteringEventListener(this);
  }
  
  @Pure
  public ConcurrentCollection<Behavior> getRegisteredBehaviors() {
    return this.behaviorDelegate.getRegisteredBehaviors();
  }
  
  @Pure
  public boolean hasRegisteredBehavior() {
    return this.behaviorDelegate.hasRegisteredBehavior();
  }
  
  @DefaultValueSource
  public Behavior registerBehavior(final Behavior attitude, @DefaultValue("io.sarl.core.Behaviors#REGISTERBEHAVIOR_0") final Function1<? super Event, ? extends Boolean> filter, final Object... initializationParameters) {
    return this.behaviorDelegate.registerBehavior(attitude, filter, initializationParameters);
  }
  
  public Behavior unregisterBehavior(final Behavior attitude) {
    return this.behaviorDelegate.unregisterBehavior(attitude);
  }
  
  @DefaultValueSource
  public void wake(final Event event, @DefaultValue("io.sarl.core.Behaviors#WAKE_0") final Scope<Address> scope) {
    this.behaviorDelegate.wake(event, scope);
  }
  
  public void wake(final Behavior beh, final Event event) {
    this.behaviorDelegate.wake(beh, event);
  }
  
  public void wake(final Iterable<Behavior> behs, final Event event) {
    this.behaviorDelegate.wake(behs, event);
  }
  
  @Override
  @Pure
  @SyntheticMember
  public boolean equals(final Object obj) {
    return super.equals(obj);
  }
  
  @Override
  @Pure
  @SyntheticMember
  public int hashCode() {
    int result = super.hashCode();
    return result;
  }
}
