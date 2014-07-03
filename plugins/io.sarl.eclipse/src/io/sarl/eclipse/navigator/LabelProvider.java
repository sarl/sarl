/**
 * 
 */
package io.sarl.eclipse.navigator;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

/**
 * SARL custom project navigator label provider
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class LabelProvider implements ILabelProvider {


    @Override
    public Image getImage(Object element) {
        Image image = null;
        
        if (ISARLProjectElement.class.isInstance(element)) {
            image = ((ISARLProjectElement)element).getImage();
        }
        // else ignore the element
        
        return image;
    }

    @Override
    public String getText(Object element) {
        String text = ""; //$NON-NLS-1$
        if (ISARLProjectElement.class.isInstance(element)) {
            text = ((ISARLProjectElement)element).getText();
        }
        // else ignore the element
        
        return text;
    }

    @Override
    public void addListener(ILabelProviderListener listener) {
        System.out.println("LabelProvider.addListener: " + listener.getClass().getName()); //$NON-NLS-1$
    }

    @Override
    public void dispose() {
        System.out.println("LabelProvider.dispose"); //$NON-NLS-1$
    }

    @Override
    public boolean isLabelProperty(Object element, String property) {
        System.out.println("LabelProvider.isLabelProperty: " + element.getClass().getName()); //$NON-NLS-1$
        return false;
    }

    @Override
    public void removeListener(ILabelProviderListener listener) {
        System.out.println("LabelProvider.removeListener: " + listener.getClass().getName()); //$NON-NLS-1$
    }
    

}
