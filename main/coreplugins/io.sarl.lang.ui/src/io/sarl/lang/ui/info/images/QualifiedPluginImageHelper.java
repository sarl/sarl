/*
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

package io.sarl.lang.ui.info.images;

import javax.inject.Singleton;

import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.xtext.ui.PluginImageHelper;

/**
 * An image loader that supports qualified names for the images ids.
 *
 * <p>The image ids could be qualified with the containing plugin id.
 *
 * <p>TODO: Remove this class when submitted and accepted by the Xtext project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class QualifiedPluginImageHelper extends PluginImageHelper {

	/** Find the descriptor of the image with the given id.
	 *
	 * @param name the identifier of the image. It may be qualified with the plugin's id.
	 * @return the descriptor.
	 */
	protected ImageDescriptor findImage(String name) {
		final ImageDescriptor descriptor = JavaPluginImages.getDescriptor(name);
		if (descriptor != null) {
			return descriptor;
		}
		if (name != null) {
			final int extIndex = name.lastIndexOf('.');
			if (extIndex > 0) {
				final int index = name.lastIndexOf('.', extIndex - 1);
				if (index > 0) {
					final String pluginId = name.substring(0, index);
					final String imageId = name.substring(index + 1);
					return AbstractUIPlugin.imageDescriptorFromPlugin(pluginId,
							getPathSuffix() + imageId);
				}
			}
		}
		return null;
	}

	@Override
	public Image getImage(String name) {
		final ImageDescriptor descriptor = findImage(name);
		if (descriptor != null) {
			return descriptor.createImage();
		}
		return super.getImage(name);
	}

	@Override
	public ImageDescriptor getImageDescriptor(String imageName) {
		final ImageDescriptor descriptor = findImage(imageName);
		if (descriptor != null) {
			return descriptor;
		}
		return super.getImageDescriptor(imageName);
	}

}
