import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.Capacity;

@FunctionalInterface
@SarlSpecification("0.12")
@SarlElementType(20)
@SuppressWarnings("all")
public interface Cap2 extends Capacity {
  void action2();
  
  /**
   * @ExcludeFromApidoc
   */
  class ContextAwareCapacityWrapper<C extends Cap2> extends Capacity.ContextAwareCapacityWrapper<C> implements Cap2 {
    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {
      super(capacity, caller);
    }
    
    public void action2() {
      try {
        ensureCallerInLocalThread();
        this.capacity.action2();
      } finally {
        resetCallerInLocalThread();
      }
    }
  }
}
