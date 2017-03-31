/**
 * $Id$
 * 
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 * 
 * Copyright (C) 2014-2017 the original authors or authors.
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
package io.sarl.experienceindex;

import io.sarl.lang.annotation.SarlSpecification;
import org.eclipse.osgi.util.NLS;
import org.eclipse.xtext.xbase.lib.Functions.Function0;

/**
 * Messages.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SarlSpecification("0.6")
@SuppressWarnings("all")
public final class Messages extends NLS {
  private final static String BUNDLE_NAME = new Function0<String>() {
    public String apply() {
      String _xblockexpression = null;
      {
        final String name = "io.sarl.experienceindex.messages";
        NLS.initializeMessages(name, Messages.class);
        _xblockexpression = name;
      }
      return _xblockexpression;
    }
  }.apply();
  
  public static String SarlExperienceIndex_0;
  
  public static String SarlExperienceIndex_1;
  
  public static String SarlExperienceIndex_2;
  
  public static String SarlExperienceIndex_3;
  
  public static String SarlExperienceIndex_4;
  
  public static String SarlExperienceIndex_5;
  
  public static String SarlExperienceIndex_6;
  
  public static String SarlExperienceIndex_7;
  
  private Messages() {
  }
}
