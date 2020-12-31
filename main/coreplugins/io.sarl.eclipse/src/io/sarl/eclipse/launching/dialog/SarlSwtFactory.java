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

package io.sarl.eclipse.launching.dialog;

import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jface.fieldassist.ControlDecoration;
import org.eclipse.jface.fieldassist.FieldDecoration;
import org.eclipse.jface.fieldassist.FieldDecorationRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;

/**
 * Additional utilities for the launch configuration tabs.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public final class SarlSwtFactory extends SWTFactory {

	private static final int DEFAULT_INCREMENT = 1;

	private static final int DEFAULT_PAGE_INCREMENT = 10;

	private SarlSwtFactory() {
		//
	}

	/** Create a spinner component.
	 *
	 * @param parent the parent component.
	 * @param hspan the horizontal span (number of columns)
	 * @param min the minimum value to be selected into the spinner.
	 * @param max the maximum value to be selected into the spinner.
	 * @param increment the increment for the selected value.
	 * @param pageIncrement the page increment for the selected value.
	 * @return the spinner.
	 */
	public static Spinner createSpinner(Composite parent, int hspan, int min, int max, int increment, int pageIncrement) {
		final Spinner spinner = new Spinner(parent, SWT.SINGLE | SWT.BORDER);
		spinner.setFont(parent.getFont());
		spinner.setMinimum(min);
		spinner.setMaximum(max);
		spinner.setIncrement(increment);
		spinner.setPageIncrement(pageIncrement);
		spinner.setDigits(0);
		final GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.horizontalSpan = hspan;
		spinner.setLayoutData(gd);
		return spinner;
	}

	/** Create a spinner component.
	 *
	 * @param parent the parent component.
	 * @param hspan the horizontal span (number of columns)
	 * @param min the minimum value to be selected into the spinner.
	 * @param max the maximum value to be selected into the spinner.
	 * @return the spinner.
	 */
	public static Spinner createSpinner(Composite parent, int hspan, int min, int max) {
		return createSpinner(parent, hspan, min, max, DEFAULT_INCREMENT, DEFAULT_PAGE_INCREMENT);
	}

	/** Attach an information decorator to the given component.
	 *
	 * @param parent the component that receives the info decorator.
	 * @param text the information message.
	 * @param location the attachment location. It may be bitwise value from {@link SWT#TOP},
	 *     {@link SWT#DOWN}, {@link SWT#LEFT}, {@link SWT#RIGHT}, {@link SWT#CENTER}.
	 * @return the decorator.
	 */
	public static ControlDecoration createInfoDecorator(Control parent, String text, int location) {
		final ControlDecoration txtDecorator = new ControlDecoration(parent, location);
		final FieldDecoration fieldDecoration = FieldDecorationRegistry.getDefault().getFieldDecoration(FieldDecorationRegistry.DEC_INFORMATION);
		final Image img = fieldDecoration.getImage();
		txtDecorator.setImage(img);
		txtDecorator.setDescriptionText(text);
		return txtDecorator;
	}

	/** Attach an information decorator to the given component at its top left corner.
	 *
	 * @param parent the component that receives the info decorator.
	 * @param text the information message.
	 * @return the decorator.
	 */
	public static ControlDecoration createInfoDecorator(Control parent, String text) {
		return createInfoDecorator(parent, text, SWT.TOP | SWT.LEFT);
	}

	/** Create a big information icon that shows up an information message in its tool tip text.
	 *
	 * @param parent the component that receives the info bubble.
	 * @param text the information message.
	 * @param hspan the number of columns occupied by the bubble icon.
	 * @return the information icon widget.
	 */
	public static Label createInfoBubble(Composite parent, String text, int hspan) {
		final FieldDecoration fieldDecoration = FieldDecorationRegistry.getDefault().getFieldDecoration(FieldDecorationRegistry.DEC_INFORMATION);
		final Image img = fieldDecoration.getImage();
		final Label txtDecorator = createLabel(parent, "", hspan); //$NON-NLS-0$
		txtDecorator.setImage(img);
		txtDecorator.setToolTipText(text);
		return txtDecorator;
	}

	/** Create a big information icon that shows up an information message in its tool tip text.
	 *
	 * @param parent the component that receives the info bubble.
	 * @param text the information message.
	 * @return the information icon widget.
	 */
	public static Label createInfoBubble(Composite parent, String text) {
		return createInfoBubble(parent, text, 1);
	}

}
