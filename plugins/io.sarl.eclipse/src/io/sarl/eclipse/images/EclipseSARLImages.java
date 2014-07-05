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

	/** Filename of the image that may be used at
	 * the top of the wiazrd dialog when creating new SARL project.
	 */
	public static final String NEW_PROJECT_WIZARD_DIALOG_IMAGE = "icons/sarl_new_project_dialog.png"; //$NON-NLS-1$	

	/** Filename of the image that may be used at
	 * the top of the wiazrd dialog when creating new SARL file.
	 */
	public static final String NEW_FILE_WIZARD_DIALOG_IMAGE = "icons/sarl_new_file_dialog.png"; //$NON-NLS-1$

	
	private static final String PLUGIN_ID = "io.sarl.eclipse"; //$NON-NLS-1$

	/** Replies the image stored in the current Eclipse plugin.
	 * 
	 * @param imagePath
	 * @return the image.
	 */
    public static Image getImage(String imagePath) {
        return getImageDescriptor(imagePath).createImage();
    }

	/** Replies the descriptor of the image stored in the current Eclipse plugin.
	 * 
	 * @param imagePath
	 * @return the image descriptor.
	 */
    public static ImageDescriptor getImageDescriptor(String imagePath) {
        return AbstractUIPlugin.imageDescriptorFromPlugin(EclipseSARLImages.PLUGIN_ID, imagePath);
        
    }

}
