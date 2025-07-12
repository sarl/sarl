/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.bspl.lang.formatting2;

import org.eclipse.xtext.formatting2.IFormattableDocument;
import org.eclipse.xtext.xbase.annotations.formatting2.XbaseWithAnnotationsFormatter;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;

import io.sarl.bspl.lang.bspl.BsplProtocol;
import io.sarl.bspl.lang.bspl.BsplProtocolSpecification;

/**
 * This class contains custom formatting description.
 *
 * <p>Developers: for avoiding formatting conflicts between two keywords, try to avoid "surrounding" and
 * use only "prepend".
 *
 * <p>The {@link FormatterFacade} provides a convenient API for formatting strings.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see FormatterFacade
 * @since 0.15
 */
public class BSPLFormatter extends XbaseWithAnnotationsFormatter {

	/** Format a protocol specification.
	 * 
	 * @param bsplProtocolSpecification the specification to format.
	 * @param doc the output.
	 */
	@SuppressWarnings("static-method")
	protected void format(BsplProtocolSpecification bsplProtocolSpecification, IFormattableDocument doc) {
		doc.format(bsplProtocolSpecification.getImportSection());
		for (final var bsplProtocol : bsplProtocolSpecification.getBsplProtocols()) {
			doc.format(bsplProtocol);
		}
	}

	/** Format a protocol.
	 * 
	 * @param bsplProtocol the specification to format.
	 * @param doc the output.
	 */
	@SuppressWarnings("static-method")
	protected void format(BsplProtocol bsplProtocol, IFormattableDocument doc) {
		// TODO: format HiddenRegions around keywords, attributes, cross references, etc. 
		for (XAnnotation xAnnotation : bsplProtocol.getAnnotations()) {
			doc.format(xAnnotation);
		}
	}
	
}
