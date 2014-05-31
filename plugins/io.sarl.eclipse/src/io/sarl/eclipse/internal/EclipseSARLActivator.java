/**
 * 
 */
package io.sarl.eclipse.internal;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;



/**
 * Activator
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class EclipseSARLActivator extends AbstractUIPlugin {


	public static final String PLUGIN_ID = "io.sarl.eclipse";

	
    public static Image getImage(String imagePath) {
    	
    	
        ImageDescriptor imageDescriptor = AbstractUIPlugin.imageDescriptorFromPlugin(EclipseSARLActivator.PLUGIN_ID, imagePath);
        Image image = imageDescriptor.createImage();
 
        return image;
    }

}
