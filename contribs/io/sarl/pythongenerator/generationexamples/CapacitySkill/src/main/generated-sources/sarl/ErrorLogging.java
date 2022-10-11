import Logging;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.AgentTrait;

@SarlSpecification("0.12")
@SarlElementType(20)
@SuppressWarnings("all")
public interface ErrorLogging extends Logging {
  void error(final String text);
  
  /**
   * @ExcludeFromApidoc
   */
  class ContextAwareCapacityWrapper<C extends ErrorLogging> extends Logging.ContextAwareCapacityWrapper<C> implements ErrorLogging {
    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {
      super(capacity, caller);
    }
    
    public void error(final String text) {
      try {
        ensureCallerInLocalThread();
        this.capacity.error(text);
      } finally {
        resetCallerInLocalThread();
      }
    }
  }
}
