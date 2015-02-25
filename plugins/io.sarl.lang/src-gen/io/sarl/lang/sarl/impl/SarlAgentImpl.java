/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.xtend.core.xtend.impl.XtendTypeDeclarationImpl;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Agent</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlAgentImpl#getExtends <em>Extends</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class SarlAgentImpl extends XtendTypeDeclarationImpl implements SarlAgent
{
  /**
   * The cached value of the '{@link #getExtends() <em>Extends</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getExtends()
   * @generated
   * @ordered
   */
  protected EList<JvmParameterizedTypeReference> extends_;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected SarlAgentImpl()
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
    return SarlPackage.Literals.SARL_AGENT;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<JvmParameterizedTypeReference> getExtends()
  {
    if (extends_ == null)
    {
      extends_ = new EObjectContainmentEList<JvmParameterizedTypeReference>(JvmParameterizedTypeReference.class, this, SarlPackage.SARL_AGENT__EXTENDS);
    }
    return extends_;
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
      case SarlPackage.SARL_AGENT__EXTENDS:
        return ((InternalEList<?>)getExtends()).basicRemove(otherEnd, msgs);
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
      case SarlPackage.SARL_AGENT__EXTENDS:
        return getExtends();
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
      case SarlPackage.SARL_AGENT__EXTENDS:
        getExtends().clear();
        getExtends().addAll((Collection<? extends JvmParameterizedTypeReference>)newValue);
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
      case SarlPackage.SARL_AGENT__EXTENDS:
        getExtends().clear();
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
      case SarlPackage.SARL_AGENT__EXTENDS:
        return extends_ != null && !extends_.isEmpty();
    }
    return super.eIsSet(featureID);
  }

} //SarlAgentImpl
