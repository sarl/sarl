import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;

@SarlSpecification("0.12")
@SarlElementType(20)
@SuppressWarnings("all")
public interface pingCapacity extends Capacity {
  void sendping();
  
  void replyPing(final Event occ);
  
  /**
   * @ExcludeFromApidoc
   */
  class ContextAwareCapacityWrapper<C extends pingCapacity> extends Capacity.ContextAwareCapacityWrapper<C> implements pingCapacity {
    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {
      super(capacity, caller);
    }
    
    public void sendping() {
      try {
        ensureCallerInLocalThread();
        this.capacity.sendping();
      } finally {
        resetCallerInLocalThread();
      }
    }
    
    public void replyPing(final Event occ) {
      try {
        ensureCallerInLocalThread();
        this.capacity.replyPing(occ);
      } finally {
        resetCallerInLocalThread();
      }
    }
  }
}
