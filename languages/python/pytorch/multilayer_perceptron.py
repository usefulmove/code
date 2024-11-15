# -*- coding: utf-8 -*-
"""multilayer_perceptron.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1vNZ0V7ZldKSNbXZAG51IxXvOo2-HOiC7

## construct model
"""

import torch
from torch import nn
from torchvision import datasets, transforms
from torchsummary import summary

# define the model
class MLPerceptron(nn.Module):
    def __init__(self):
        super(MLPerceptron, self).__init__()
        input_size = 28 * 28
        hidden_size = 128
        output_size = 10
        self.layer1 = nn.Linear(input_size, hidden_size)
        self.layer2 = nn.Linear(hidden_size, hidden_size)
        self.layer3 = nn.Linear(hidden_size, hidden_size)
        self.layer4 = nn.Linear(hidden_size, output_size)

    def forward(self, x):
        x = torch.relu(self.layer1(x))
        x = torch.relu(self.layer2(x))
        x = torch.relu(self.layer3(x))
        x = self.layer4(x)
        return x

# create GPU device (if available)
device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
print(device)

# instantiate the model
model = MLPerceptron() # multi-layer perceptron model

# move the model to the GPU (if available)
model.to(device)

summary(model, (1, 28 * 28))

# define a transform to normalize the data
transform = transforms.Compose([transforms.ToTensor(), transforms.Normalize((0.5,), (0.5,))])

# download and load the data
dataset = datasets.MNIST('~/.pytorch/MNIST_data/', download=True, train=True, transform=transform)

"""## model training"""

from torch import optim
from torch.utils.data import DataLoader, random_split

# number of epochs
n_epochs = 30

# split the dataset into training and validation sets
training_size = int(0.8 * len(dataset))  # 80% for training
validation_size = len(dataset) - training_size  # 20% for validation
training_data, validation_data = random_split(dataset, [training_size, validation_size])

# create training and validation data loaders
training_loader = DataLoader(training_data, batch_size=64, shuffle=True)
validation_loader = DataLoader(validation_data, batch_size=64, shuffle=True)

# define loss criterion and optimizer
loss_criterion = nn.MSELoss() # mean squared error loss function
optimizer = optim.SGD(model.parameters(), lr=0.8) # stochaistic gradient descent

# training and validation
for epoch in range(n_epochs):
    # training
    model.train()
    training_loss = 0.0
    for inputs, labels in training_loader:
        # move inputs and labels to GPU (if available)
        inputs, labels = inputs.to(device), labels.to(device)

        # zero the parameter gradients
        optimizer.zero_grad()

        # reshape the inputs and forward pass
        inputs = inputs.view(inputs.shape[0], -1)
        outputs = model(inputs)

        # transform labels to match the output shape
        labels = nn.functional.one_hot(labels, num_classes=10).float()

        # calculate loss
        loss = loss_criterion(outputs, labels)
        training_loss += loss.item()

        # backward pass and optimization
        loss.backward()
        optimizer.step()

    # calculate average loss over an epoch
    training_loss = training_loss / len(training_loader)

    # validation loss
    model.eval()
    validation_loss = 0.0
    with torch.no_grad():
        for inputs, labels in validation_loader:
            # move inputs and labels to GPU (if available)
            inputs, labels = inputs.to(device), labels.to(device)

            inputs = inputs.view(inputs.shape[0], -1)
            outputs = model(inputs)
            labels = nn.functional.one_hot(labels, num_classes=10).float()
            loss = loss_criterion(outputs, labels)
            validation_loss += loss.item()
    validation_loss = validation_loss / len(validation_loader)        

    print('epoch: {} \ttraining loss: {:.6f} \tvalidation loss: {:.6f}'.format(epoch+1, training_loss, validation_loss))

"""## model validation"""

# validation
model.eval()
correct = 0
total = 0
error_inputs = []
error_labels = []
error_predictions = []

with torch.no_grad():
    for inputs, labels in validation_loader:
        # move inputs and labels to GPU (if available)
        inputs, labels = inputs.to(device), labels.to(device)

        inputs = inputs.view(inputs.shape[0], -1)
        outputs = model(inputs)
        
        # get the predicted class for each sample in the batch
        _, predicted = torch.max(outputs, 1)
        
        # count total number of labels and correct predictions
        total += labels.size(0)
        correct += (predicted == labels).sum().item()

        # store inputs, labels, and predictions where predictions are incorrect
        for i, label in enumerate(labels):
            if label != predicted[i]:
                error_labels.append(label.item())
                error_inputs.append(inputs[i])
                error_predictions.append(predicted[i].item())

# calculate the percentage of correct predictions
accuracy = correct / total * 100

print('model accuracy: {:.2f}% ({:d} of {:d})'.format(accuracy, correct, total))

"""## plot example of a failed prediction"""

import matplotlib.pyplot as plt
from torchvision.transforms.functional import to_pil_image

ind = 0
input = error_inputs[ind]
label = error_labels[ind]
prediction = error_predictions[ind]

print(input.size())

# unnormalize and reshape the image
image_tensor = (input * 0.5 + 0.5).reshape((28, 28))

# convert to PIL Image
image = to_pil_image(image_tensor)

print(f'model prediction: {prediction} \tlabel: {label}')

# display the image
plt.imshow(image, cmap='gray')
plt.show()

"""## save the model weights and biases"""

# save the model
from google.colab import drive
drive.mount('/content/gdrive')

torch.save({
    'model_state_dict': model.state_dict(),
    'optimizer_state_dict': optimizer.state_dict(),
}, "/content/gdrive/My Drive/model.pth")