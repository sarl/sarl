import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.Capacity;

@SarlSpecification("0.12")
@SarlElementType(20)
@SuppressWarnings("all")
public interface Logging extends Capacity {
  void info(final String text);
  
  void debug(final String text);
  
  /**
   * @ExcludeFromApidoc
   */
  class ContextAwareCapacityWrapper<C extends Logging> extends Capacity.ContextAwareCapacityWrapper<C> implements Logging {
    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {
      super(capacity, caller);
    }
    
    public void info(final String text) {
      try {
        ensureCallerInLocalThread();
        this.capacity.info(text);
      } finally {
        resetCallerInLocalThread();
      }
    }
    
    public void debug(final String text) {
      try {
        ensureCallerInLocalThread();
        this.capacity.debug(text);
      } finally {
        resetCallerInLocalThread();
      }
    }
  }
}
