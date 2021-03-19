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
import com.hazelcast.collection.ItemListener;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.topic.ITopic;
import com.hazelcast.topic.MessageListener;
import io.sarl.core.OpenEventSpace;
import io.sarl.core.OpenEventSpaceSpecification;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.SpaceID;
import io.sarl.sre.network.services.HazelcastEventTransportService;
import io.sarl.sre.services.context.Context;
import io.sarl.sre.services.context.ContextService;
import io.sarl.sre.services.logging.LoggingService;
import io.sarl.tests.api.Nullable;
import io.sarl.tests.api.tools.TestAssertions;
import java.util.Objects;
import java.util.UUID;
import java.util.logging.Logger;
import org.eclipse.xtext.xbase.lib.Pure;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("unit: HazelcastEventTransportService test")
@Tag("unit")
@Tag("janus")
@Tag("sre-unit")
@Tag("sre-network")
@SuppressWarnings("raw_type")
@SarlSpecification("0.12")
@SarlElementType(10)
public class HazelcastEventTransportServiceTest {
  @Nullable
  private HazelcastEventTransportService ets;
  
  @Nullable
  private HazelcastInstance hazelcast;
  
  @Nullable
  private ContextService context;
  
  @Nullable
  private LoggingService logging;
  
  @Nullable
  private ISet internalSet;
  
  @Nullable
  private ITopic internalTopic;
  
  @Nullable
  private String expectedTopicName;
  
  @Nullable
  private SpaceID spaceId;
  
  @BeforeEach
  public void setUp() {
    final UUID cid = UUID.randomUUID();
    final UUID sid = UUID.randomUUID();
    SpaceID _spaceID = new SpaceID(cid, sid, OpenEventSpaceSpecification.class);
    this.spaceId = _spaceID;
    this.expectedTopicName = ((("io.sarl.topics.space." + cid) + ".") + sid);
    this.internalSet = Mockito.<ISet>mock(ISet.class);
    this.internalTopic = Mockito.<ITopic>mock(ITopic.class);
    final Answer<Object> _function = (InvocationOnMock it) -> {
      return UUID.randomUUID();
    };
    Mockito.<UUID>when(this.internalTopic.addMessageListener(ArgumentMatchers.<MessageListener>any())).thenAnswer(_function);
    this.hazelcast = Mockito.<HazelcastInstance>mock(HazelcastInstance.class);
    Mockito.<ISet<Object>>when(this.hazelcast.<Object>getSet(ArgumentMatchers.anyString())).thenReturn(this.internalSet);
    Mockito.<ITopic<Object>>when(this.hazelcast.<Object>getReliableTopic(this.expectedTopicName)).thenReturn(this.internalTopic);
    final OpenEventSpace rootSpace = Mockito.<OpenEventSpace>mock(OpenEventSpace.class);
    Mockito.<SpaceID>when(rootSpace.getSpaceID()).thenReturn(this.spaceId);
    final Context rootContext = Mockito.<Context>mock(Context.class);
    Mockito.<OpenEventSpace>when(rootContext.getDefaultSpace()).thenReturn(rootSpace);
    this.context = Mockito.<ContextService>mock(ContextService.class);
    Mockito.<Context>when(this.context.getRootContext()).thenReturn(rootContext);
    final Logger logger = Mockito.<Logger>mock(Logger.class);
    this.logging = Mockito.<LoggingService>mock(LoggingService.class);
    Mockito.<Logger>when(this.logging.getKernelModuleLogger(ArgumentMatchers.anyString())).thenReturn(logger);
    this.ets = Mockito.<HazelcastEventTransportService>spy(new HazelcastEventTransportService(
      this.hazelcast, 
      this.context, 
      this.logging));
  }
  
  @Test
  @DisplayName("initialization of the service")
  public void initialization() {
    final ArgumentCaptor<ItemListener> listener = ArgumentCaptor.<ItemListener, ItemListener>forClass(ItemListener.class);
    Mockito.<ISet>verify(this.internalSet).addItemListener(listener.capture(), ArgumentMatchers.eq(true));
    Assertions.assertNotNull(listener.getValue());
    final ArgumentCaptor<String> name = ArgumentCaptor.<String, String>forClass(String.class);
    Mockito.<ISet>verify(this.internalSet).add(name.capture());
    Assertions.assertEquals(this.expectedTopicName, name.getValue());
    final ArgumentCaptor<MessageListener> listener1 = ArgumentCaptor.<MessageListener, MessageListener>forClass(MessageListener.class);
    Mockito.<ITopic>verify(this.internalTopic).addMessageListener(listener1.capture());
    Assertions.assertNotNull(listener1.getValue());
  }
  
  @Test
  @DisplayName("getTopicNameFromSpaceID")
  public void getTopicNameFromSpaceID() {
    Assertions.assertEquals(this.expectedTopicName, HazelcastEventTransportService.getTopicNameFromSpaceID(this.spaceId));
  }
  
  @SuppressWarnings("unused_local_variable")
  @Test
  @DisplayName("routeEvent")
  public void routeEvent() {
    final Event event = Mockito.<Event>mock(Event.class);
    final EventSpace space = Mockito.<EventSpace>mock(EventSpace.class);
    Mockito.<SpaceID>when(space.getSpaceID()).thenReturn(this.spaceId.clone());
    final Scope scope = Mockito.<Scope>mock(Scope.class);
    final boolean result = this.ets.routeEvent(event, space, scope);
    Assertions.assertTrue(result);
    final ArgumentCaptor<Object> capmsg = ArgumentCaptor.<Object, Object>forClass(Object.class);
    Mockito.<ITopic>verify(this.internalTopic).publish(capmsg.capture());
    final Object v = capmsg.getValue();
    Assertions.assertNotNull(v);
    TestAssertions.assertInstanceOf(HazelcastEventTransportService.TopicMessage.class, v);
    final HazelcastEventTransportService.TopicMessage tm = ((HazelcastEventTransportService.TopicMessage) v);
    Assertions.assertSame(event, tm.getTransferredEvent());
    Assertions.assertSame(scope, tm.getTrasnferredScope());
  }
  
  @Override
  @Pure
  @SyntheticMember
  public boolean equals(final Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    HazelcastEventTransportServiceTest other = (HazelcastEventTransportServiceTest) obj;
    if (!Objects.equals(this.expectedTopicName, other.expectedTopicName))
      return false;
    return super.equals(obj);
  }
  
  @Override
  @Pure
  @SyntheticMember
  public int hashCode() {
    int result = super.hashCode();
    final int prime = 31;
    result = prime * result + Objects.hashCode(this.expectedTopicName);
    return result;
  }
  
  @SyntheticMember
  public HazelcastEventTransportServiceTest() {
    super();
  }
}
