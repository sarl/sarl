import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;

@SarlSpecification("0.12")
@SarlElementType(20)
@SuppressWarnings("all")
public interface pongCapacity extends Capacity {
  void sendpong();
  
  void replyPong(final Event occ);
  
  /**
   * @ExcludeFromApidoc
   */
  class ContextAwareCapacityWrapper<C extends pongCapacity> extends Capacity.ContextAwareCapacityWrapper<C> implements pongCapacity {
    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {
      super(capacity, caller);
    }
    
    public void sendpong() {
      try {
        ensureCallerInLocalThread();
        this.capacity.sendpong();
      } finally {
        resetCallerInLocalThread();
      }
    }
    
    public void replyPong(final Event occ) {
      try {
        ensureCallerInLocalThread();
        this.capacity.replyPong(occ);
      } finally {
        resetCallerInLocalThread();
      }
    }
  }
}
