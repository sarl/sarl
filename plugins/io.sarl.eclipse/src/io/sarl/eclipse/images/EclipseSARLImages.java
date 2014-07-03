/**
 * 
 */
package io.sarl.eclipse.images;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;



/**
 * Images
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class EclipseSARLImages {

	private static final String PLUGIN_ID = "io.sarl.eclipse"; //$NON-NLS-1$

	/** Replies the image stored in the current Eclipse plugin.
	 * 
	 * @param imagePath
	 * @return the image.
	 */
    public static Image getImage(String imagePath) {
        ImageDescriptor imageDescriptor = AbstractUIPlugin.imageDescriptorFromPlugin(EclipseSARLImages.PLUGIN_ID, imagePath);
        Image image = imageDescriptor.createImage();
        return image;
    }

}
