package io.sarl.demos.gameoflife.environment.agent;

import io.sarl.demos.gameoflife.environment.agent.GridManager;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.Skill;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.UUID;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * @author Jérôme BOULMIER
 */
@SarlSpecification("0.4")
@SuppressWarnings("all")
public class DefaultGridManagerSkill extends Skill implements GridManager {
  protected final Random random = new Random();
  
  protected final int width;
  
  protected final int height;
  
  protected final List<List<UUID>> grid = CollectionLiterals.<List<UUID>>newArrayList();
  
  public DefaultGridManagerSkill(final int width, final int height) {
    this.width = width;
    this.height = height;
  }
  
  @Override
  @Pure
  public List<List<Pair<UUID, Boolean>>> getPerceptions() {
    Object _xblockexpression = null;
    {
      final ArrayList<Object> perceptions = CollectionLiterals.<Object>newArrayList();
      ArrayList<Object> perception = CollectionLiterals.<Object>newArrayList();
      _xblockexpression = null;
    }
    return ((List<List<Pair<UUID, Boolean>>>)_xblockexpression);
  }
  
  @Override
  @Pure
  public int getWidth() {
    return this.width;
  }
  
  @Override
  @Pure
  public int getHeight() {
    return this.height;
  }
}
