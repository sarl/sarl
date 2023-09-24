/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.eclipse.experienceindex;

import java.lang.ref.SoftReference;
import java.text.MessageFormat;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IconAndMessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import io.sarl.api.experienceindex.SarlExperienceIndex.SEI;

/**
 * Dialog that shows the results for the SARL experience index.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class SarlExperienceIndexResultDialog extends IconAndMessageDialog {

	private static final int GLOBAL_ICON_SIZE = 256;

	private static final String BACKGROUND_IMAGE = "icons/sei_background.png"; //$NON-NLS-1$

	private static final String ICON = "icons/sei.png"; //$NON-NLS-1$

	private static final String SEI_FONT = "arial"; //$NON-NLS-1$

	private static final int SEI_FONT_SIZE = 110;

	private static final int SEI_FONT_STYLE = SWT.BOLD;

	private static final int SEI_FONT_COLOR_RED = 94;

	private static final int SEI_FONT_COLOR_GREEN = 105;

	private static final int SEI_FONT_COLOR_BLUE = 114;

	private final SEI sei;

	private SoftReference<Image> globalImage;

	/** Construct the dialog.
	 *
	 * @param parentShell the parent shell.
	 * @param sei the Sarl experience index.
	 */
	public SarlExperienceIndexResultDialog(Shell parentShell, SEI sei) {
		super(parentShell);
		this.sei = sei;
		this.message = MessageFormat.format(
				Messages.SarlExperienceIndexResultDialog_0,
				Float.valueOf(sei.getBaseScore()),
				Float.valueOf(sei.getCpuScore()),
				Float.valueOf(sei.getMemoryScore()),
				Float.valueOf(sei.getDiskScore()));
	}

	/**
	 * Opens a SEI dialog to display the results of the SEI computation.
	 *
	 * @param parent the parent shell of the dialog, or {@code null} if none.
	 * @param sei the Sarl experience index.
	 * @return the code of the button that was pressed that resulted in this
	 *         dialog closing. This will be {@link Dialog#OK} if the OK
	 *         button was pressed, or {@link Dialog#CANCEL} if this
	 *         dialog's close window decoration or the ESC key was used.
	 */
	public static int open(Shell parent, SEI sei) {
		final SarlExperienceIndexResultDialog dialog = new SarlExperienceIndexResultDialog(parent, sei);
		return dialog.open();
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);
		createMessageArea(composite);
		return composite;
	}

	@Override
	protected Image getImage() {
		Image img = this.globalImage != null ? this.globalImage.get() : null;
		if (img == null) {
			final Display display = getShell().getDisplay();
			img = new Image(display, GLOBAL_ICON_SIZE, GLOBAL_ICON_SIZE);
			final GC gc = new GC(img);
			final Image backimg = SarlExperienceIndexPlugin.getDefault().getImage(BACKGROUND_IMAGE);
			gc.setAntialias(SWT.ON);
			gc.drawImage(backimg, 0, 0);
			backimg.dispose();
			gc.setForeground(new Color(display, SEI_FONT_COLOR_RED, SEI_FONT_COLOR_GREEN, SEI_FONT_COLOR_BLUE));
			final Font newFont = new Font(display, SEI_FONT, SEI_FONT_SIZE, SEI_FONT_STYLE);
			gc.setFont(newFont);
			final String text = MessageFormat.format(Messages.SarlExperienceIndexResultDialog_1,
					Float.valueOf(this.sei.getBaseScore()));
			final Point size = gc.textExtent(text);
			gc.drawText(text,
					(GLOBAL_ICON_SIZE - size.x) / 2,
					(GLOBAL_ICON_SIZE - size.y) / 2,
					SWT.DRAW_TRANSPARENT);
			newFont.dispose();
			gc.dispose();
			this.globalImage = new SoftReference<>(img);
		}
		return img;
	}

	@Override
	protected void configureShell(Shell shell) {
		super.configureShell(shell);
		shell.setText(Messages.SarlExperienceIndexResultDialog_2);
		shell.setImage(SarlExperienceIndexPlugin.getDefault().getImage(ICON));
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
	}
	
}
