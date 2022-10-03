import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.Capacity;

@FunctionalInterface
@SarlSpecification("0.12")
@SarlElementType(20)
@SuppressWarnings("all")
public interface c extends Capacity {
  void dothing(final int thing);
  
  /**
   * @ExcludeFromApidoc
   */
  class ContextAwareCapacityWrapper<C extends c> extends Capacity.ContextAwareCapacityWrapper<C> implements c {
    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {
      super(capacity, caller);
    }
    
    public void dothing(final int thing) {
      try {
        ensureCallerInLocalThread();
        this.capacity.dothing(thing);
      } finally {
        resetCallerInLocalThread();
      }
    }
  }
}
