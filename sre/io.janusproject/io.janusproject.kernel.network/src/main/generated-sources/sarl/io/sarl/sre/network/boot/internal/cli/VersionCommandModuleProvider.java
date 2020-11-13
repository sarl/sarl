/**
 * $Id$
 * 
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 * 
 * Copyright (C) 2014-2020 the original authors or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License")
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
package io.sarl.sre.network.boot.internal.cli;

import io.bootique.BQModule;
import io.bootique.BQModuleProvider;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.annotation.SyntheticMember;
import io.sarl.sre.network.boot.internal.cli.Messages;
import io.sarl.sre.network.boot.internal.cli.VersionCommandModule;
import java.util.Collection;
import java.util.Collections;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;

/**
 * Provider of the module for the version command.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
@SarlSpecification("0.12")
@SarlElementType(10)
@SuppressWarnings("all")
public class VersionCommandModuleProvider implements BQModuleProvider {
  @Override
  public com.google.inject.Module module() {
    return new VersionCommandModule();
  }
  
  @Override
  public Collection<Class<? extends com.google.inject.Module>> overrides() {
    return Collections.<Class<? extends com.google.inject.Module>>unmodifiableList(CollectionLiterals.<Class<? extends com.google.inject.Module>>newArrayList(io.sarl.sre.boot.internal.cli.VersionCommandModule.class));
  }
  
  @Override
  public BQModule.Builder moduleBuilder() {
    return BQModule.builder(this.module()).overrides(this.overrides()).providerName(this.name()).configs(this.configs()).description(
      Messages.VersionCommandModuleProvider_0);
  }
  
  @SyntheticMember
  public VersionCommandModuleProvider() {
    super();
  }
}
