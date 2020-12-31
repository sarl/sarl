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

package io.sarl.eclipse.splash;

import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.internal.splash.EclipseSplashHandler;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.lang.SARLVersion;

/**
 * Dynamic splash screen that add the "BETA" picture when the product is in unstable state.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see io.sarl.lang.SARLConfig
 */
public class SarlSplashHandler extends EclipseSplashHandler {

	private static final String BETA_PNG = "icons/splash/beta-decoration.png"; //$NON-NLS-1$

	private static final int X = 0;

	private static final int Y = 0;

	private Image image;

	@Override
	public void init(Shell splash) {
		super.init(splash);

		if (this.image != null) {
			this.image.dispose();
			this.image = null;
		}
		if (!SARLVersion.IS_STABLE) {
			this.image = SARLEclipsePlugin.getDefault().getImage(BETA_PNG);
			if (this.image != null) {
				final Rectangle betaRectangle = new Rectangle(X, Y,
						this.image.getImageData().width, this.image.getImageData().height);
				final Label betaCtrl = new Label(getContent(), 0);
				betaCtrl.setBackgroundImage(this.image);
				betaCtrl.setBounds(betaRectangle);
			}
		}
		// Do the event until. Ensure that the RCP is stopped until the splash UI has done.
		doEventLoop();
	}

	private void doEventLoop() {
		final Shell splash = getSplash();
		if (!splash.getDisplay().readAndDispatch()) {
			splash.getDisplay().sleep();
		}
	}

	@Override
    public void dispose() {
        super.dispose();
        if (this.image != null) {
            this.image.dispose();
            this.image = null;
        }
    }

}
