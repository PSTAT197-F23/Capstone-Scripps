# -*- coding: utf-8 -*-
"""
Created on Sun Feb 11 18:01:09 2024

@author: Michaela Alksne

# script to train faster rCNN model 
# this is for the Sonobuoys!!!!

# the pretrained Faster R-CNN ResNet-50 model that we are going to use expects the input image tensor to be in the form [c, h, w], where:

# c is the number of channels, for RGB images its 3 (which is what I have rn)
# h is the height of the image
# w is the width of the image

"""

import pandas as pd
from PIL import Image, ImageDraw
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import Rectangle
import torch
import torchvision
from torchvision import transforms as T
from torchvision.models.detection import FasterRCNN
from torchvision.models.detection.rpn import AnchorGenerator
from torch.utils.data import Dataset
from torch.utils.data import DataLoader
import torch.optim as optim
from torchvision.models.detection.faster_rcnn import FastRCNNPredictor
import sys
#sys.path.append(r"C:\\Users\\aryye\\OneDrive\\Documents\\GitHub\\WhaleMoanDetector\\code\\PYTHON")
#from AudioDetectionDataset_MNA_grayscale import AudioDetectionData
from AudioDetectionDataset import AudioDetectionData

        
def custom_collate(data):
    return data # 
# this just allows you to return the datasets as is. 
# because we won't always have the same number of bounding boxes..

'''
train_d1 = DataLoader(AudioDetectionData(csv_file="C:\\Users\\aryye\\OneDrive\\Documents\\GitHub\\WhaleMoanDetector\\labeled_data\\train_val_test_annotations\\train_partial.csv"),
                      batch_size=16,
                      shuffle = True,
                      collate_fn = custom_collate, 
                      pin_memory = True if torch.cuda.is_available() else False)
'''

train_d1 = DataLoader(AudioDetectionData(csv_file="C:\\Users\\aryye\\OneDrive\\Documents\\GitHub\\WhaleMoanDetector\\labeled_data\\Modified Annotations\\train\\Processed_Training_Images\\modified_annotations.csv"),
                      batch_size=16,
                      shuffle = True,
                      collate_fn = custom_collate, 
                      pin_memory = True if torch.cuda.is_available() else False)
                     

#val_d1 = DataLoader(AudioDetectionData(csv_file='L:\\Sonobuoy_faster-rCNN\\labeled_data\\train_val_test_annotations\\validation.csv'),
#                      batch_size=1,
#                      shuffle = False,
#                      collate_fn = custom_collate,
#                      pin_memory = True if torch.cuda.is_available() else False)
        
        
# load our model
model = torchvision.models.detection.fasterrcnn_resnet50_fpn(pretrained=True)
num_classes = 6 # two classes plus background
in_features = model.roi_heads.box_predictor.cls_score.in_features # classification score and number of features (1024 in this case)

# I might need to change in features
model.roi_heads.box_predictor = FastRCNNPredictor(in_features,num_classes)  
        

device = torch.device('cuda') if torch.cuda.is_available() else torch.device('cpu')

optimizer = torch.optim.SGD(model.parameters(), lr = 0.001, momentum = 0.9, weight_decay= 0.0005) #SDG = stochastic gradient desent with these hyperparameters
num_epochs = 15


# model training loop.
'''
model.to(device)
for epochs in range(num_epochs):
    model.train() # Set the model to training mode
    epoch_train_loss  = 0
    for data in train_d1:
        imgs = []
        targets = []
        for d in data:
            imgs.append(d[0].to(device)) #we have to send each image from the dataloader to our cpu..
            targ = {}
            targ['boxes'] = d[1]['boxes'].to(device)
            targ['labels'] = d[1]['labels'].to(device)
            targets.append(targ)
            
        loss_dict = model(imgs,targets)
        loss = sum(v for v in loss_dict.values())
        epoch_train_loss += loss.cpu().detach().numpy()
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()
    print(f'training loss: {epoch_train_loss}')
    #model_save_path = f'L:\\Sonobuoy_faster-rCNN\\trained_model\\WhaleMoanDetector_{epochs}.pth'
    model_save_path = f"C:\\Users\\aryye\\OneDrive\\Documents\\GitHub\\WhaleMoanDetector\\labeled_data\\trained_model\\WhaleMoanDetector_{epochs}.pth"
    torch.save(model.state_dict(), model_save_path)
    '''
model.to(device)
for epoch in range(num_epochs):
    model.train()  # Set the model to training mode
    epoch_train_loss = 0
    for batch_idx, data in enumerate(train_d1):
        imgs = []
        targets = []
        for d in data:
            imgs.append(d[0].to(device))
            targ = {'boxes': d[1]['boxes'].to(device),
                    'labels': d[1]['labels'].to(device)}
            targets.append(targ)

        try:
            loss_dict = model(imgs, targets)
            loss = sum(loss for loss in loss_dict.values())
            epoch_train_loss += loss.cpu().detach().numpy()

            optimizer.zero_grad()
            loss.backward()
            optimizer.step()

            print(f'Epoch {epoch + 1}, Batch {batch_idx}, Training Loss: {loss.item()}')
        except Exception as e:
            print(f"Error in batch {batch_idx}: {e}")
            continue  # Skip this batch and continue with the next one

    print(f'Epoch {epoch + 1}, Training Loss: {epoch_train_loss}')
    model_save_path = f"C:\\Users\\aryye\\OneDrive\\Documents\\GitHub\\WhaleMoanDetector\\labeled_data\\trained_model\\WhaleMoanDetector_{epoch}.pth"
    torch.save(model.state_dict(), model_save_path)

    
    
    
    
    # if I also want to run the validation data but idk how to do that lol
    # Set the model to evaluation mode
    #epoch_val_loss = 0
    #with torch.no_grad():  # No need to track gradients during validation
    '''
        for data in val_d1:
            imgs = []
            targets = []
            for d in data:
                imgs.append(d[0].to(device))
                targ = {}
                targ['boxes'] = d[1]['boxes'].to(device)
                targ['labels'] = d[1]['labels'].to(device)
                targets.append(targ)
                val_loss_dict = model(imgs, targets)
                val_losses = sum(v for v in val_loss_dict.values())
                epoch_val_loss += val_losses.cpu().detach().numpy()
    print(f"Epoch {epochs}, Train Loss: {epoch_train_loss}, Validation Loss: {epoch_val_loss}")


#final_model_path = 'L:\\Sonobuoy_faster-rCNN\\trained_model\\Sonobuoy_model_final.pth'
final_model_path = "C:\\Users\\aryye\\OneDrive\\Documents\\GitHub\\WhaleMoanDetector\\labeled_data\\trained_model\\Sonobuoy_model_final.pth"
torch.save(model.state_dict(), final_model_path)
        
        
        
        
        
        
        
        
'''