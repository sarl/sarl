/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlSkill;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.xtend.core.xtend.XtendMember;

import org.eclipse.xtend.core.xtend.impl.XtendTypeDeclarationImpl;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Skill</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlSkillImpl#getSuperTypes <em>Super Types</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlSkillImpl#getImplementedTypes <em>Implemented Types</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlSkillImpl#getFeatures <em>Features</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class SarlSkillImpl extends XtendTypeDeclarationImpl implements SarlSkill
{
  /**
   * The cached value of the '{@link #getSuperTypes() <em>Super Types</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getSuperTypes()
   * @generated
   * @ordered
   */
  protected EList<JvmParameterizedTypeReference> superTypes;

  /**
   * The cached value of the '{@link #getImplementedTypes() <em>Implemented Types</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getImplementedTypes()
   * @generated
   * @ordered
   */
  protected EList<JvmParameterizedTypeReference> implementedTypes;

  /**
   * The cached value of the '{@link #getFeatures() <em>Features</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getFeatures()
   * @generated
   * @ordered
   */
  protected EList<XtendMember> features;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected SarlSkillImpl()
  {
    super();
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  protected EClass eStaticClass()
  {
    return SarlPackage.Literals.SARL_SKILL;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<JvmParameterizedTypeReference> getSuperTypes()
  {
    if (superTypes == null)
    {
      superTypes = new EObjectContainmentEList<JvmParameterizedTypeReference>(JvmParameterizedTypeReference.class, this, SarlPackage.SARL_SKILL__SUPER_TYPES);
    }
    return superTypes;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<JvmParameterizedTypeReference> getImplementedTypes()
  {
    if (implementedTypes == null)
    {
      implementedTypes = new EObjectContainmentEList<JvmParameterizedTypeReference>(JvmParameterizedTypeReference.class, this, SarlPackage.SARL_SKILL__IMPLEMENTED_TYPES);
    }
    return implementedTypes;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<XtendMember> getFeatures()
  {
    if (features == null)
    {
      features = new EObjectContainmentEList<XtendMember>(XtendMember.class, this, SarlPackage.SARL_SKILL__FEATURES);
    }
    return features;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
  {
    switch (featureID)
    {
      case SarlPackage.SARL_SKILL__SUPER_TYPES:
        return ((InternalEList<?>)getSuperTypes()).basicRemove(otherEnd, msgs);
      case SarlPackage.SARL_SKILL__IMPLEMENTED_TYPES:
        return ((InternalEList<?>)getImplementedTypes()).basicRemove(otherEnd, msgs);
      case SarlPackage.SARL_SKILL__FEATURES:
        return ((InternalEList<?>)getFeatures()).basicRemove(otherEnd, msgs);
    }
    return super.eInverseRemove(otherEnd, featureID, msgs);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public Object eGet(int featureID, boolean resolve, boolean coreType)
  {
    switch (featureID)
    {
      case SarlPackage.SARL_SKILL__SUPER_TYPES:
        return getSuperTypes();
      case SarlPackage.SARL_SKILL__IMPLEMENTED_TYPES:
        return getImplementedTypes();
      case SarlPackage.SARL_SKILL__FEATURES:
        return getFeatures();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @SuppressWarnings("unchecked")
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SarlPackage.SARL_SKILL__SUPER_TYPES:
        getSuperTypes().clear();
        getSuperTypes().addAll((Collection<? extends JvmParameterizedTypeReference>)newValue);
        return;
      case SarlPackage.SARL_SKILL__IMPLEMENTED_TYPES:
        getImplementedTypes().clear();
        getImplementedTypes().addAll((Collection<? extends JvmParameterizedTypeReference>)newValue);
        return;
      case SarlPackage.SARL_SKILL__FEATURES:
        getFeatures().clear();
        getFeatures().addAll((Collection<? extends XtendMember>)newValue);
        return;
    }
    super.eSet(featureID, newValue);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eUnset(int featureID)
  {
    switch (featureID)
    {
      case SarlPackage.SARL_SKILL__SUPER_TYPES:
        getSuperTypes().clear();
        return;
      case SarlPackage.SARL_SKILL__IMPLEMENTED_TYPES:
        getImplementedTypes().clear();
        return;
      case SarlPackage.SARL_SKILL__FEATURES:
        getFeatures().clear();
        return;
    }
    super.eUnset(featureID);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public boolean eIsSet(int featureID)
  {
    switch (featureID)
    {
      case SarlPackage.SARL_SKILL__SUPER_TYPES:
        return superTypes != null && !superTypes.isEmpty();
      case SarlPackage.SARL_SKILL__IMPLEMENTED_TYPES:
        return implementedTypes != null && !implementedTypes.isEmpty();
      case SarlPackage.SARL_SKILL__FEATURES:
        return features != null && !features.isEmpty();
    }
    return super.eIsSet(featureID);
  }

} //SarlSkillImpl
