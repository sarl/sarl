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

import io.bootique.log.BootLogger;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.sre.SreVersion;
import io.sarl.sre.network.boot.internal.cli.Messages;
import java.text.MessageFormat;

/**
 * Command for showing the SRE version with the network feature inside.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@SarlSpecification("0.12")
@SarlElementType(10)
@SuppressWarnings("all")
public class VersionCommand extends io.sarl.maven.bootiqueapp.version.VersionCommand {
  /**
   * Constructor.
   * 
   * @param bootLogger the logger.
   */
  public VersionCommand(final BootLogger bootLogger) {
    super(bootLogger, VersionCommand.class, Messages.VersionCommand_0);
  }
  
  @Override
  protected String getVersionMessage() {
    final String inheritedMessage = super.getVersionMessage();
    return MessageFormat.format(Messages.VersionCommand_1, 
      SreVersion.RELEASE_VERSION, 
      Boolean.valueOf(SreVersion.IS_STABLE), inheritedMessage);
  }
}
