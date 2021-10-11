/*
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

package io.sarl.docs.doclet.j11.config;

import com.google.inject.Injector;
import jdk.javadoc.doclet.Doclet;
import jdk.javadoc.doclet.DocletEnvironment;
import jdk.javadoc.internal.doclets.formats.html.HtmlConfiguration;
import jdk.javadoc.internal.doclets.toolkit.WriterFactory;

import io.sarl.docs.doclet.j11.factories.UtilsFactory;
import io.sarl.docs.doclet.j11.utils.SarlJavadocUtils;
import io.sarl.docs.doclet.j11.writers.SarlWriterFactory;

/** Configuration for the SARL doclet.
 *
 * <p>This class provides the following features in addition to the standard API:<ul>
 * <li>Force the writer factory to be of type {@link SarlWriterFactory}</li>
 * <li>Force the utils to be of type {@link SarlJavadocUtils}</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class SarlHtmlConfiguration extends HtmlConfiguration {

	private final Injector injector;
	
    /**
     * Creates an object to hold the configuration for a doclet.
     *
     * @param doclet the container.
     * @param injector the injector.
     */
    public SarlHtmlConfiguration(Doclet doclet, Injector injector) {
    	super(doclet);
    	this.injector = injector;
    }
    
    @Override
    protected void initConfiguration(DocletEnvironment docEnv) {
    	super.initConfiguration(docEnv);
        // Reset Utils (needs docEnv, safe to reset now)
        this.utils = this.injector.getInstance(UtilsFactory.class).create(this);
    }

    @Override
    public WriterFactory getWriterFactory() {
    	return this.injector.getInstance(WriterFactory.class);
    }

}
