/**
 * $Id$
 * 
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 * 
 * Copyright (C) 2014-2021 the original authors or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.sre.network.tests.units.services;

import com.hazelcast.collection.ISet;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.map.IMap;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.sre.boot.configs.SreConfig;
import io.sarl.sre.boot.configs.subconfigs.BootConfig;
import io.sarl.sre.internal.Factories;
import io.sarl.sre.network.services.HazelcastContextService;
import io.sarl.sre.network.tests.units.services.mocks.IMapMock;
import io.sarl.sre.services.context.Context;
import io.sarl.sre.tests.framework.SreTestUtilities;
import io.sarl.sre.tests.framework.units.services.context.AbstractInjectionBasedContextServiceTest;
import io.sarl.tests.api.Nullable;
import java.util.UUID;
import javax.inject.Provider;
import org.eclipse.xtext.xbase.lib.Pure;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("unit: HazelcastContextService test")
@Tag("unit")
@Tag("janus")
@Tag("sre-unit")
@Tag("sre-network")
@SarlSpecification("0.12")
@SarlElementType(10)
@SuppressWarnings("all")
public class HazelcastContextServiceTest extends AbstractInjectionBasedContextServiceTest<HazelcastContextService> {
  @Nullable
  private SreConfig sreConfig;
  
  @Nullable
  private HazelcastInstance hazelcast;
  
  @SuppressWarnings("raw_type")
  @Nullable
  private ISet internalSet;
  
  @Nullable
  private IMap<UUID, UUID> istructure;
  
  @BeforeEach
  @Override
  public void setUp() {
    final UUID cid = UUID.randomUUID();
    final UUID sid = UUID.randomUUID();
    final BootConfig bootConfig = Mockito.<BootConfig>mock(BootConfig.class);
    Mockito.<UUID>when(bootConfig.getRootContextID()).thenReturn(cid);
    Mockito.<UUID>when(bootConfig.getRootSpaceID()).thenReturn(sid);
    this.sreConfig = Mockito.<SreConfig>mock(SreConfig.class);
    Mockito.<BootConfig>when(this.sreConfig.getBoot()).thenReturn(bootConfig);
    IMapMock<UUID, UUID> _iMapMock = new IMapMock<UUID, UUID>();
    this.istructure = _iMapMock;
    this.internalSet = Mockito.<ISet>mock(ISet.class);
    this.hazelcast = Mockito.<HazelcastInstance>mock(HazelcastInstance.class);
    Mockito.<ISet<Object>>when(this.hazelcast.<Object>getSet(ArgumentMatchers.anyString())).thenReturn(this.internalSet);
    Mockito.<IMap<UUID, UUID>>when(this.hazelcast.<UUID, UUID>getMap(ArgumentMatchers.anyString())).thenReturn(this.istructure);
    super.setUp();
  }
  
  @Override
  public HazelcastContextService newService(final Context rootContext) {
    final Provider<Factories> _function = () -> {
      return SreTestUtilities.newFactories();
    };
    return new HazelcastContextService(
      this.sreConfig, rootContext, 
      this.loggingService, 
      this.injector, 
      this.contextFactory, _function, 
      this.hazelcast);
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
  
  @SyntheticMember
  public HazelcastContextServiceTest() {
    super();
  }
}
