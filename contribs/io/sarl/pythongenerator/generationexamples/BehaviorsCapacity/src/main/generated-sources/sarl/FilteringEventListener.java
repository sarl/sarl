import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import java.util.UUID;
import org.eclipse.xtext.xbase.lib.Pure;

@SarlSpecification("0.12")
@SarlElementType(10)
@SuppressWarnings("all")
public class FilteringEventListener implements EventListener {
  private final FilteringEventDispatchingBehavior parent;
  
  public FilteringEventListener(final FilteringEventDispatchingBehavior parent) {
    this.parent = parent;
  }
  
  @Override
  public void receiveEvent(final Event occ) {
    boolean _isInstance = this.parent.acceptedType.isInstance(occ);
    if (_isInstance) {
      this.parent.behaviorDelegate.asEventListener().receiveEvent(occ);
    }
  }
  
  @Pure
  @Override
  public UUID getID() {
    return this.parent.getOwner().getID();
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
